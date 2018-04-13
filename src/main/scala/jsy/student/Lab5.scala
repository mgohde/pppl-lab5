package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * Michael Gohde
   *
   * Partner: Alex Urbanski
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env, e1).map{ ee1 => Unary(uop, ee1)}

      case Binary(bop, e1, e2) => ren(env, e1).flatMap({
        ee1 => ren(env, e2).map({
          ee2 => Binary(bop, ee1, ee2)
        })})

      case If(e1, e2, e3) => ren(env, e1).flatMap({
        ee1 => ren(env, e2).flatMap({
          ee2 => ren(env, e3).map({
            ee3 => If(ee1, ee2, ee3)
          })
        })
      })

      case Var(x) => if(env.contains(x)) doreturn(Var(env(x))) else doreturn(Var(x))

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        ren(env, e1).flatMap({
          ee1 => ren(env+(x->xp), e2).map({
            ee2 => Decl(m, xp, ee1, ee2)
          })
        })
      }

      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => ???
          case Some(x) => ???
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
            case ((x,mty), acc) => acc flatMap {
              ???
            }
          } flatMap {
            ???
          }
        }
      }

      case Call(e1, args) => ???

      case Obj(fields) => ???
      case GetField(e1, f) => ???

      case Assign(e1, e2) => ???

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    //We need to do two things:
    //1. Generate a new string
    //2. Increment the value of this object's state.
    val fresh: String => DoWith[Int,String] = { w: String => doget map {W: Int => "x"+W}//new DoWith[Int, String]({_:Any => (w+1, "x"+w)})
    }
    val (_, r) = rename(empty, e)(fresh)(???)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      //Our initial accumulator is a DoWith with an empty internal list and empty state.
      case (v, acc) => acc.flatMap({(nv) => f(v).map({(nnv) => nnv::nv})}) // acc.map({a:List[B] => f(v)::a})
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map.empty) ) {
      case (v, acc) => acc.flatMap({(nv:Map[C, D]) => f(v).map({ case (a, b) => nv + (a -> b)})})
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(Nil)
    case h :: t => f(h) match {
      case Some(dw) => dw.map({a:A => a::t})
        //Well, I'll be damned! This actually works!
      case None => mapFirstWith(t)(f).map({a:List[A] => h::a})
    }
  }

  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => f(h) match {
      case Some(s) => s::t
      case None => h::mapFirst(t)(f)
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    case (TNumber, TNumber) | (TString, TString) | (TBool, TBool) | (TUndefined, TUndefined) => true
    case (TNull, TObj(contents)) => contents==Map.empty
    //case _ => ???
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = ???

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => ???
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) =>
        ???
      case Binary(Minus|Times|Div, e1, e2) => {
        val t1=typeof(env, e1)
        val t2=typeof(env, e2)

        (t1, t2) match {
          case (TNumber, TNumber) => TNumber
          case (_, _) => err(TNumber, e)
        }
      }
      case Binary(Eq|Ne, e1, e2) => {
        //Y'know what? I could probably just use (typeof(), typeof()) match {}
        val t1=typeof(env, e1)
        val t2=typeof(env, e2)

        if(t1==t2) TBool else err(t1, e)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => {
        val t1=typeof(env, e1)
        val t2=typeof(env, e2)

        (t1, t2) match {
          case (TNumber, TNumber) => TBool
          case (_, _) => err(TBool, e)
        }
      }
      case Binary(And|Or, e1, e2) =>{
        val t1=typeof(env, e1)
        val t2=typeof(env, e2)

        (t1, t2) match {
          case (TBool, TBool) => TBool
          case (_, _) => err(TBool, e)
        }
      }
      case Binary(Seq, e1, e2) => typeof(env, e2)
      case If(e1, e2, e3) => {
        val t1=typeof(env, e1)
        val t2=typeof(env, e2)
        val t3=typeof(env, e3)

        t1 match{
          case TBool => (t2, t3) match {
            case (TBool, TBool) => TBool
            case (TNumber, TNumber) => TNumber
            case (TString, TString) => TString
            case (TUndefined, TUndefined) => TUndefined
            case (_, _) => err(TUndefined, e)
          }
          case _ => err(t1, e)
        }
      }
        /*
      case Function(p, params, tann, e1) => {
        // Mtyp allows us to distinguish between const and name declarations (ie. consts and pointers?)
        // Bind to env1 an environment that extends env with an appropriate binding if <- THIS IS IMPORTANT! If the function calls itself, we need to add its name to the type environment.
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case(None, None) => env //Neither name nor type; env should just be passed through.
          case(None, Some(ftype)) => env //Has a type but no name
          case(Some(fname), Some(ftype)) => env ++ Map(fname -> ftype)//Has a name and type. Do we now need to add a binding for this function?

          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = env1++params //TODO: Extract types from the MTyp list and properly extend env2!
        // Infer the type of the function body
        val t1 = typeof(env1, e1)

        // Check with the possibly annotated return type
        tann match {
          case Some(ftype) => if(ftype==t1) t1 else err(t1, e1)
          case None => t1
        }
      }*/
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params zip args).foreach { // Each element in params is matched and extended with an element in args.
            zipped => zipped match {
              case ((pname, pmtyp), pmexpr) => if(pmtyp.t!=typeof(env, pmexpr)) err(typeof(env, pmexpr), e)
              case _ => err(TUndefined, e)
            }
          };
          tret
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields.map(arg => arg match {
        case (s, expr) => (s, typeof(env, expr))
        case _ => err(TUndefined, e)
      }))
      case GetField(e1, f) => e1 match {
        case Obj(fields) => fields.get(f) match {
          case Some(x) => typeof(env, x)
          case None => err(TUndefined, e)
        }
        case _ => err(TUndefined, e)
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) =>
        ???
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            ???
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = ???
        // Match on whether the return type is specified.
        tann match {
          case None => ???
          case Some(tret) => ???
        }
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            ???
          }
          tret
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) =>
        ???
      case Assign(GetField(e1, f), e2) =>
        ???
      case Assign(_, _) => err(TUndefined, e)

      case Null =>
        ???

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if ??? => ???
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (N(a), N(b)) => bop match {
        case Lt => a<b
        case Le => a<=b
        case Gt => a>b
        case Ge => a>=b
      }
      // Todo: implement this for strings.
      //case _ => ??? // delete this line when done
      case _ => false
    }
  }

  def findInParams(x: String, params: List[(String, MTyp)]): Option[MTyp] = {
    params.foreach(v => v match {
      case (s, m) => if(s==x) m
    })

    None
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if(y.equals(x)) esub else e //I don't think we can pass this on.
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, params, tann, e1) => p match{
        case None => findInParams(x, params) match {
          case Some(m) => e
          case None => Function(p, params, tann, substitute(e1, esub, x))
        }
        case Some(fn) => if(x!=fn) e else Function(p, params, tann, substitute(e1, esub, x))
        //case _ => ???
      }
        /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(substitute(e1, esub, x), args.map(av => substitute(av, esub, x))) // We should also sub on args...
      case Obj(fields) => Obj(fields.map(args => args match {
        case (fname, fexpr) => (fname, substitute(fexpr, esub, x))
      }))
      case GetField(e1, f) => GetField(substitute(e1, esub, x), f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(???){ x => ??? }
    }

    subst(???)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode.
   * true = the expression can be further reduced (???)
   * false = the expression cannot be further reduced.*/
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => !isValue(e) //This is a reduced expression if e is a value.
    case MName => false //To my knowledge we (always?) use the squiggly arrow fella here.
    case MVar => !isValue(e) //This is a reduced expression if e is a value under the ValMode rule.
    case MRef => !isLValue(e)
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
        //This should be the ConstBind rule. Under this, we return the value to which e steps or so
      case MConst => doreturn(e)
        //This should be the NameBind rule. Under this rule, we just give M, e:
      case MName => doreturn(e)
        //Requirement: a is not in the domain of Mem
        //Return: Mem such that address a maps to value v; pointer address a.
      case MVar => memalloc(e) map { v => Unary(Deref, A(v.addr))}
        //Requirement: none
        //Return: Just mem and lvalue.
      case MRef => doreturn(e)
    }
  }

  def getFirstNonValue(m: Map[String, Expr]): (String, Expr) = {
    var e:Expr=N(0)
    var s:String=""
    var b:Boolean=false

    m.foreach{ case(k, v) => if(!isValue(v) && !b) {e=v; s=k; b=true}}

    return (s, e)
  }

  def getFirstNonValueList(a: List[Expr]): Expr = {
    var relem:Expr=N(0)
    var b:Boolean=false

    a foreach {
      elem => if(!isValue(elem) && !b) {relem=elem; b=true}
    }

    relem
  }

  def strGt(a: String, b: String): Boolean = {
    var i=0;

    for(i <- 0 to math.min(a.size, b.size)) {
      if(a.charAt(i)>b.charAt(i)) true else if(a.charAt(i)<b.charAt(i)) false
    }

    false
  }

  def allAreValsList(elist: List[Expr]): Boolean = {
    elist.foreach(e => if(!isValue(e)) false)

    true
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, v1) if isValue(v1) => doget map {m => v1 match {
        case N(v) => N(-v)
      }}

      case Unary(Deref, v1) if isValue(v1) => v1 match {
        case A(addr) => doget map(m => m.get(A(addr)) match {
          case Some(v) => v
          case None => throw NullDereferenceError(e)
        } )
        case _ => ???
      }

      case Unary(Neg, v1) if isValue(v1) => v1 match {
        case N(n) => doget map { m => N(-n) }
      }

      case Unary(Not, v1) if isValue(v1) => v1 match {
        case B(n) => doget map { m => B(!n) }
      }

      case Unary(Cast(t), v1) if isValue(v1) => (t, v1) match {
        case (_, Null) => doget map { m => Null }
        case(TNumber, N(n)) => doget map { m => v1}
        case(TNumber, S(n)) => doget map { m => v1}
        case(TNumber, B(n)) => doget map { m => if(n) N(1) else N(0)}

      }

      case Binary(binop, v1, v2) if isValue(v1) && isValue(v2) => binop match {
        case Plus => (v1, v2) match {
          case (N(a), N(b)) => doget map { m=> N(a+b) }
          case (S(a), S(b)) => doget map { m=> S(a+b) }
        }

        case Minus => (v1, v2) match {
          case (N(a), N(b)) => doget map { m=> N(a-b) }
        }

        case Times => (v1, v2) match {
          case (N(a), N(b)) => doget map { m=> N(a*b) }
        }

        case Div => (v1, v2) match {
          case (N(a), N(b)) => doget map { m=> N(a/b) }
        }

          //TODO: Boolean inequalities?

        case Gt | Lt | Ge | Le | Eq | Ne => (v1, v2) match {
          case (N(a), N(b)) => doget map { m => B(inequalityVal(binop, v1, v2))}
            //Sssshhhhh! Ignore the horrible inefficiency.
          case (S(a), S(b)) => binop match {
            case Eq => doget map { m => B(a==b) }
            //3 cheers for only having to implement one comparison function.
            case Gt => doget map { m => B(strGt(a, b)) }
            case Lt => doget map { m => B(!strGt(a, b) && a!=b) }
            case Ge => doget map { m => B(strGt(a, b) || a==b) }
            case Le => doget map { m => B(!strGt(a, b)) }
            case Ne => doget map { m => B(a!=b) }
          }
        }
      }

        //DoSeq:
      case Binary(Seq, v1, e2) if isValue(v1) => doget map { m => e2 }
      case Binary(And, v1, e2) if isValue(v1) => v1 match {
        case B(b) => if(b) doget map { m => e2 } else doget map { m => v1 }
      }
      case Binary(Or, v1, e2) if isValue(v1) => v1 match {
        case B(b) => if(b) doget map { m => v1 } else doget map { m => e2}
      }

      case If(v1, e2, e3) if isValue(v1) => v1 match {
        case B(b) => if(b) doget map { m => e2 } else doget map { m => e3 }
          //Todo: add type checking, no?
      }



        /***** More cases here */
        /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => memalloc(e)

      case GetField(a @ A(_), f) => {
        doget map { m => m.get(a) match {
          case Some(expr) => expr match {
            case Obj(fields) => fields.get(f) match {
              case Some(field) => field
              case _ => ???
            }
          }
          case _ => ???
        }}
      }

        //Do rules for decls:
      case Decl(MConst, x, v1, e2) if isValue(v1) => doget map { m => substitute(e2, v1, x) }
      case Decl(MVar, x, v1, e2) if isValue(v1) => memalloc(v1) map { a => substitute(e2, Unary(Deref, a), x)}
        /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) => {
        doget map { m => m.get(a) match {
          case Some(ex) => ex
          case None => throw NullDereferenceError(e) //Throw some kind of error
        }}
      }

        //This should be a DoAssignVar:
      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m+(a->v) } map { _ => v }

        //DoAssignField:
      case Assign(GetField(a @ A(_), f), v) if isValue(v) => doget map { m => {m + (a->v); v}}

      case Call(v1, args) if isValue(v1) => //Fundamental idea: all elements in the call get replaced with the contents of args.
        v1 match {
          case Function(p, params, _, e1) => {
            val pazip = params zip args
            //What we should *actually* be doing here is checking for values.
            if (allAreValsList(args)) {
              val e1p = pazip.foldRight(e1) {
                //We're going from last to first
                case (((s: String, mt: MTyp), e: Expr), b: Expr) => substitute(e, b, s)
              }
              p match {
                case None => doget map { m => e1p }
                case Some(x1) => doget map { m => substitute(e1p, v1, x1) } //Put the function into itself if it's recursive.
              }
            }
            else {

              //This... might do it.
              val ex = getFirstNonValueList(args)

              doget flatMap { m =>
                step(ex) map { exp => {
                  //args(ex)=exp
                  Call(e1, args.map { case ex => exp; case somev => somev })
                }
                }
              }

              /*
              //Assumption: These are all our call by value function parameters?
              val pazipp = mapFirst(pazip) {
                case ((s: String, mt: MTyp), e:Expr) => if(!isValue(e)) Some(((s, mt), step(e))) else None
              }

              val newExprs=pazipp.map {
                case((s: String, mt: MTyp), e:Expr) => e
              }

              Call(v1, newExprs)
            }*/
            }
          }
          case _ => throw StuckError(e)
        }

/*
      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args

        //This is probably the recursive DoCall case:
        if (allAreValsList(params)) {
          val dwep = pazip.foldRight( ??? : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => ???
          }
          p match {
            case None => ???
            case Some(x) => ???
          }
        }
          //This is probably the non-recursive DoCall case:
        else {
          val dwpazipp = mapFirstWith(pazip) {
            ???
          }
          ???
        }
      }*/

      /* Base Cases: Error Rules */
        /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)

      /* Inductive Cases: Search Rules */
        /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => step(e1) map {e1p => Unary(uop, e1p)}
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map {e2p => Binary(bop, v1, e2p)}
      case Binary(bop, e1, e2) => step(e1) map {e1p => Binary(bop, e1p, e2)}
      case If(e1, e2, e3) => step(e1) map {e1p => If(e1p, e2, e3)}
        /***** Cases needing adapting from Lab 4 */
        //SearchGetField:
      case GetField(e1, f) => step(e1) map { e1p => GetField(e1p, f)}
        //SearchObj
      case Obj(fields) => {
        val (n, ex)=getFirstNonValue(fields)

        doget flatMap{ m => step(ex) map {exp => {
          //fields(n)=exp //Update the new expression in the field
          Obj(fields+(n -> exp))
        }}}
      }

        //SearchDecl requires that e1 be reducible under the given mode.
      case Decl(mode, x, e1, e2) if(isRedex(mode, e1)) =>  step(e1) map { e1p => Decl(mode, x, e1p, e2)}
        //SearchCall2: Determine if all args not redex under given modes:
      case Call(e1, args) => {
        val ex=getFirstNonValueList(args)

        doget flatMap{ m => step(ex) map {exp => {
          //args(ex)=exp
          Call(e1, args.map{case ex => exp; case somev => somev})
        }}}
      }

        /***** New cases for Lab 5.  */
        //These are the searchassign rules:
      case Assign(e1, e2) if !isLValue(e1) => {
        step(e1) map { e1p => Assign(e1p, e2) }
      }

      case Assign(e1, e2) => {
        step(e2) map { e2p => Assign(e1, e2p) }
        //doget map { m => e2 }
      }

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
