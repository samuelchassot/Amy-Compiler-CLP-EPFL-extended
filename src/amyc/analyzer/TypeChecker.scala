package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Equals(lhs, rhs) => {
          // HINT: Take care to implement the specified Amy semantics
          val newTypeVariable = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ::: genConstraints(lhs, newTypeVariable) ::: genConstraints(rhs, newTypeVariable)
        }
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {

            pat match{
              case WildcardPattern() => (Nil, Map.empty)
              case IdPattern(name) => {
                val newType = TypeVariable.fresh()
                (Constraint(newType, scrutExpected, pat.position)::Nil, Map[Identifier, Type](name -> newType) )
              }
              case LiteralPattern(value) => (genConstraints(value, scrutExpected), Map.empty)

              case CaseClassPattern(qname, args) => {
                val constructor = table.getConstructor(qname).get
                val constrConstraint = Constraint(constructor.retType, scrutExpected, pat.position)
                val handledArgs = args.zip(constructor.argTypes).map(p => handlePattern(p._1, p._2))

                val constraintsList = constrConstraint :: handledArgs.flatMap(_._1)
                val mapName = handledArgs.flatMap(_._2)

                (constraintsList, mapName.toMap)
              }
            }

          }
          val newTypeForBody = TypeVariable.fresh()
          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ::: genConstraints(cse.expr, newTypeForBody)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st)) ++ topLevelConstraint(newTypeForBody)

        case Plus(lhs, rhs) => topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Minus(lhs, rhs) => topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Times(lhs, rhs) => topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Div(lhs, rhs) => topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Mod(lhs, rhs) => topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case LessThan(lhs, rhs) => topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) => topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case And(lhs, rhs) => topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) => topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
        case Concat(lhs, rhs) => topLevelConstraint(StringType) ::: genConstraints(lhs, StringType) ::: genConstraints(rhs, StringType)
        case Not(ex) => topLevelConstraint(BooleanType) ::: genConstraints(ex, BooleanType)
        case Neg(ex) => topLevelConstraint(IntType) ::: genConstraints(ex, IntType)

        case Sequence(exp1, exp2) => {
          val expType = TypeVariable.fresh()
          genConstraints(exp1, TypeVariable.fresh()) ::: genConstraints(exp2, expType) ::: topLevelConstraint(expType)
        }
        case Call(qname, args) => {
          val fun = table.getFunction(qname)
          val constr = table.getConstructor(qname)
          if (fun.isDefined){
            val handledArgs = args.zip(fun.get.argTypes).map(p => genConstraints(p._1, p._2))
            topLevelConstraint(fun.get.retType) ::: handledArgs.flatten
          } else {
            val handledArgs = args.zip(constr.get.argTypes).map(p => genConstraints(p._1, p._2))
            topLevelConstraint(constr.get.retType) ::: handledArgs.flatten
          }

        }
        case Let(df, value, body) => {
          val newType = TypeVariable.fresh()
          val valType = df.tt.tpe
          topLevelConstraint(newType) ::: genConstraints(value, valType) ::: genConstraints(body, newType)(Map(df.name -> valType) ++ env)
        }

        case Ite(cond, thenn, elze) => {
          val newType = TypeVariable.fresh()
          topLevelConstraint(newType) ::: genConstraints(cond, BooleanType) ::: genConstraints(thenn, newType) ::: genConstraints(elze, newType)
        }
        case Error(msg) => {
          val newType = TypeVariable.fresh()
          topLevelConstraint(newType) ::: genConstraints(msg, StringType)
        }
        case Variable(name) => {
          val variableType = env.get(name).get
          topLevelConstraint(variableType)
        }
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          found match{
            case tv : TypeVariable => solveConstraints(subst_*(more, tv.id, expected))
            case t : Type => {
              expected match{
                case tvExpec : TypeVariable => solveConstraints(Constraint(tvExpec, t, pos) :: more)
                case tExpec : Type => {
                  if(t == expected) solveConstraints(more) else {
                    val foundName = t.toString()
                    val expectedName = expected.toString()
                    ctx.reporter.error(s"type $foundName does not conform expected type $expectedName", pos)
                  }
                }
              }

            }
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
