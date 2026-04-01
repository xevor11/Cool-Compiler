import scala.annotation.tailrec

object Constants {
  val io = new IO();

  val any_sym = io.symbol("Any")
  val null_sym = io.symbol("Null")
  val unit_sym = io.symbol("Unit")
  val int_sym = io.symbol("Int")
  val nothing_sym = io.symbol("Nothing")
  val boolean_sym = io.symbol("Boolean")
  val string_sym = io.symbol("String")
  val arrayany_sym = io.symbol("ArrayAny")
  val io_sym = io.symbol("IO")
  val native_sym = io.symbol("native")
  val main_sym = io.symbol("Main")
  val symbol_sym = io.symbol("Symbol")
  val this_sym = io.symbol("this")
  val super_sym = io.symbol("super")
  val basic_cool_sym = io.symbol("/usr/local/lib/basic.cool")
  val invalid_types = Seq(nothing_sym, null_sym, native_sym)
}

class CircularInheritanceCheck(var classes: Classes) extends CoolTreeVisitor() {

  import Constants._

  // Map would be more efficient maybe something like Map[Symbol, List[Symbol]
  var class_relationships: Seq[(Symbol, Symbol)] = Seq()

  override def visit_Classes_one(node: Class): Any = node.accept(this);
  override def visit_class_decl(cd: Cclass_decl, name: Symbol, parent: Symbol, features: Features, filename: Symbol): Any = {
    // map += (name -> parent);
    // Since I am using an immutable list
    class_relationships = class_relationships :+ (name -> parent)
  }

  private def isCyclic[A](edges: Seq[(A, A)]): Seq[A] = {
    @tailrec
    def isCyclic(className: A, visited: Set[A]): Boolean = {
      if (visited(className)) return true

      parent(className) match {
        case Some(value) => isCyclic(value, visited ++ Set(className))
        case None        => false
      }
    }

    // Refactored
    def parent(name: A): Option[A] = {
      var i = 0
      var result: Option[A] = None
      while (i < edges.length && result.isEmpty) {
        val (currentName, parentName) = edges(i)
        if (currentName == name) {
          result = Some(parentName)
        }
        i += 1
      }
      result
    }

    var classNames = Set.empty[A]
    for ((className, parentName) <- edges) {
      classNames = classNames + className + parentName
    }
    classNames.filter(className => isCyclic(className, Set())).toSeq
  }

  { classes.accept(this) };

  // Refactored
  def checkClassesCircular(): Seq[Symbol] = {
    val entries: Seq[(Symbol, Symbol)] = class_relationships
    isCyclic(entries)
  };
}

class CheckParentExist(var classes: Classes) extends CoolTreeVisitor() {

  import Constants._

  var relationships: Seq[(Symbol, Symbol)] = Seq()

  override def visit_Classes_one(node: Class): Any = node.accept(this)

  override def visit_class_decl(cd: Cclass_decl, name: Symbol, parent: Symbol, features: Features, filename: Symbol): Any = {
    relationships :+= (name -> parent)
  }

  { classes.accept(this) }

  def check(): Set[Symbol] = {
    val result = findChildrenWithoutParent()
    result
  }

  private def findChildrenWithoutParent(): Set[Symbol] = {
    var result: Set[Symbol] = Set()
    for ((child, parent) <- relationships) {
      if (parent != native_sym) {
        var hasParent = false
        for ((_, p) <- relationships) {
          if (p == parent) {
            hasParent = true
          }
        }
        if (!hasParent) {
          result += child
        }
      }
    }
    result
  }
}

class FeatureEnvironment(var classes: Classes) extends CoolTreeVisitor() {

  import Constants._

  var classToClassDecl: Map[Symbol, Cclass_decl] = Map()
  var parentChildMap: Map[Symbol, Symbol] = Map();

  var mapAttributes: Map[Symbol, Map[Symbol, Feature]] = Map()
  var mapMethods: Map[Symbol, Map[Symbol, Feature]] = Map()

  var flag = true;
  var current_class: Symbol = null

  override def visit_Classes_one(node: Class): Any = node.accept(this);
  override def visit_class_decl(cd: Cclass_decl, name: Symbol, parent: Symbol, features: Features, filename: Symbol): Any = {
    if (flag) {
      classToClassDecl += (name -> cd)
      parentChildMap += (name -> parent);
    } else {
      current_class = name;

      mapAttributes.get(parent) match {
        case Some(attrs) => {
          mapAttributes += (name -> attrs)
        }
        case None => {
          mapAttributes += (name -> Map())
        }
      }

      mapMethods.get(parent) match {
        case Some(methods) => {
          mapMethods += (name -> methods)
        }
        case None => {
          mapMethods += (name -> Map())
        }
      }

      features.accept(this);
    }
  }

  def parent(name: Symbol): Option[Symbol] = {
    var result: Option[Symbol] = None
    val iterator = parentChildMap.iterator
    while (iterator.hasNext && result.isEmpty) {
      val (key, value) = iterator.next()
      if (key == name) {
        result = Some(value)
      }
    }
    result
  }

  def topologicalSort[A](edges: Seq[(A, A)]): Iterable[A] = {
    def findNoPreds(toPreds: Map[A, Set[A]]): Map[A, Set[A]] = {
      var noPreds = Map[A, Set[A]]()
      for ((key, preds) <- toPreds) {
        var noPred = true
        for (pred <- preds) {
          if (toPreds.contains(pred)) {
            noPred = false
          }
        }
        if (noPred) {
          noPreds += key -> preds
        }
      }
      noPreds
    }

    var toPred = Map[A, Set[A]]()
    for ((source, dest) <- edges) {
      if (!toPred.contains(source)) {
        toPred += source -> Set()
      }
      if (!toPred.contains(dest)) {
        toPred += dest -> Set()
      }
      toPred += dest -> (toPred(dest) + source)
    }

    var result = Seq[A]()
    while (toPred.nonEmpty) {
      val noPreds = findNoPreds(toPred)
      if (noPreds.isEmpty) {
        /* do nothing */
      } else {
        val found = noPreds.keys.toSeq
        result = result ++ found
        for ((key, preds) <- toPred) {
          toPred += key -> (preds -- found)
        }
        toPred = toPred.filterKeys(!noPreds.contains(_)).toMap
      }
    }
    result
  }

  flag = true
  classes.accept(this)
  flag = false
  val entries = parentChildMap.toSeq
  var sortedClasses = topologicalSort(entries).toList.reverse
  var i = 0
  while (i < sortedClasses.length) {
    val c = sortedClasses(i)
    if (c != native_sym) {
      classToClassDecl.get(c) match {
        case Some(classDecl) => classDecl.accept(this)
        case None => println(s"No ClassDecl found for symbol $c")
      }
    }
    i += 1
  }

  override def visit_attr(the_node: Cattr, name: Symbol, of_type: Symbol): Any = {
    var updatedAttributes: Map[Symbol, Feature] = null
    if (mapAttributes.contains(current_class)) {
      updatedAttributes = mapAttributes(current_class) + (name -> the_node)
    } else {
      updatedAttributes = Map(name -> the_node)
    }
    mapAttributes = mapAttributes + (current_class -> updatedAttributes)
  }

  override def visit_method(m: Cmethod, overridep: Boolean, name: Symbol, formals: Formals, return_type: Symbol, body: Expression): Unit = {
    var updatedMethods: Map[Symbol, Feature] = null
    if (mapMethods.contains(current_class)) {
      val existingMethods = mapMethods(current_class)
      updatedMethods = existingMethods + (name -> m)
    } else {
      updatedMethods = Map(name -> m)
    }
    mapMethods = mapMethods + (current_class -> updatedMethods)
  }

  def getClassAttributes(c: Symbol): Option[Map[Symbol, Feature]] = {
    mapAttributes.get(c)
  };

  def getClassMethods(c: Symbol): Option[Map[Symbol, Feature]] = {
    mapMethods.get(c)
  };
}

class Semant(var program: Program, var options: CoolOptions)
    extends CoolVisitor() {
  import Constants._

  var io: IO = new IO();
  var errors: SemantErrors = new SemantErrors(options);
  var class_table: ClassTable = null;

  def run(): Boolean = {
    program.accept(this);
    !errors.has_errors()
  };

  var any_sym: Symbol = io.symbol("Any");

  // #(
  def consym(s: String, sym: Symbol): String =
    s.concat(sym.toString().concat("'"));

  var nothing_sym: Symbol = io.symbol("Nothing");
  var symbol_sym: Symbol = io.symbol("Symbol");
  var native_sym: Symbol = io.symbol("native");
  var null_sym: Symbol = io.symbol("Null");

  var any_class_info: ClassInfo = null;
  var root_environment: Environment = null;
  var feature_tables_ready: Boolean = false;
  // #)

  var features_env: FeatureEnvironment = null;

  override def visit_program(the_node: Cprogram, classes: Classes): Any = {
    class_table = new ClassTable(classes, errors);

    val circularClasses =
      new CircularInheritanceCheck(classes).checkClassesCircular();
    if (!circularClasses.isEmpty) {
      return err(the_node, f"classes are circular ${circularClasses}")
    }

    val classesExtendingUndefined = new CheckParentExist(classes).check()
    if (!classesExtendingUndefined.isEmpty) {
      return err(
        the_node,
        f"classes are extending undefined ${classesExtendingUndefined}"
      )
    }

    features_env = new FeatureEnvironment(classes);
    classes.accept(this);
    // In PA4 - you need to set program attributes
    // #(
    // In PA5 - you need to arrange checks for circular inheritance etc,
    //          check for Main
    the_node.set_any_class(class_table.lookup_class(any_sym));
    the_node.set_int_class(class_table.lookup_class(int_sym));
    the_node.set_unit_class(class_table.lookup_class(unit_sym));
    the_node.set_boolean_class(class_table.lookup_class(bool_sym));
    the_node.set_string_class(class_table.lookup_class(string_sym));

    // #)
    ()
  };

  override def visit_Classes_one(node: Class): Any = node.accept(this);
  override def visit_class_decl(cd: Cclass_decl, name: Symbol, parent: Symbol, features: Features, filename: Symbol): Any = {
    // for PA4, check that name is not illegal
    // #(
    if (name == nothing_sym)
      errors.error(cd, cd, "Cannot declare class Nothing")
    else if (name == null_sym)
      errors.error(cd, cd, "Cannot declare class Null")
    else ();
    // for PA5, check superclass, inheritance
    // #)
    {
      current_class = cd;
      var parentClass = class_table.lookup_class(parent)
      // TODO PA4/PA5: set current_env
      current_env = new NestedEnvironment(
        new RootEnvironment(class_table, errors)
      );

      for ((key, value) <- features_env.getClassAttributes(name).get) {
        current_env = new SingleEnvironment(key, value, current_env)
      }

      for ((key, value) <- features_env.getClassMethods(name).get) {
        current_env = new SingleEnvironment(key, value, current_env)
      }
      // val combinedFeatures = features_env.getClassAttributes(name).get ++ features_env.getClassMethods(name).get

      // combinedFeatures.foreach { case (key, value) =>
      //   current_env = new SingleEnvironment(key, value, current_env)
      // }

      current_env = new SingleEnvironment(
        this_sym,
        class_table.lookup_class(name),
        current_env
      );
      if (parentClass != null) {
        current_env = new SingleEnvironment(
          super_sym,
          class_table.lookup_class(parent),
          current_env
        );
        // TODO: set superclass attribute
        current_class.set_superclass(parentClass)
      }

      features.accept(this)
    }
  };

  var current_class: Cclass_decl = null;
  var current_env: Environment = null;

  def err(loc: CoolNode, message: String): Unit =
    errors.error(current_class, loc, message);

  override def visit_Features_one(f: Feature): Any = {
    // ##PA5: TODO: set "owner" attribute
    f.accept(this)
  };

  override def visit_attr(the_node: Cattr, name: Symbol, of_type: Symbol): Any = {
    // #(
    the_node.set_owner(current_class)
    var of_class: Cclass_decl = class_table.lookup_class(of_type);
    if (of_type == nothing_sym)
      err(the_node, "attributes may not be given type Nothing")
    else if (of_type == null_sym) ()
    else if (is_null(of_class))
      err(the_node, consym("class of attribute undeclared: ", of_type))
    else the_node.set_feature_of_class(of_class)
    // #)
    // PA4: TODO: check types, set attributes
  };

  override def visit_method(m: Cmethod, overridep: Boolean, name: Symbol, formals: Formals, return_type: Symbol, body: Expression): Unit = {
    // #(
    if (overridep) {
      var parentMethods: Map[Symbol, Feature] = features_env.getClassMethods(current_class.get_parent()).get;

      var parentMethod: Cmethod = parentMethods.get(name) match {
        case Some(method) => method match {
          case method_name: Cmethod => method_name
        }
        case None => {
          err(m, "this method overrides a method that doesn't exist");
          null
        }
      }
      m.set_overrides(parentMethod)
    }

    m.set_owner(current_class)

    table_env = new TableEnvironment(current_env);
    formals.accept(this);
    check_expr(body, table_env);
    var of_class: Cclass_decl = class_table.lookup_class(return_type);
    m.set_feature_of_class(of_class);
    if (is_null(of_class))
      if (return_type == nothing_sym) ()
      else if (return_type == null_sym) ()
      else err(m, consym("return type undeclared: ", return_type))
    else {};
    // PA5: check that overridden method (if any) is correctly overridden
    ()
    // #)
    // PA4: TODO: set up the current environment (with formals), check types
    //      check that body type checks,
  };

  // #(

  var table_env: TableEnvironment = null;

  override def visit_Formals_one(f: Formal): Any = f.accept(this);

  override def visit_formal(f: Cformal, name: Symbol, of_type: Symbol): Any = {
    if (!is_null(table_env.probe(name)))
      err(f, consym("redeclared formal ", name))
    else table_env.add(name, f);
    var of_class: Cclass_decl = class_table.lookup_class(of_type);
    if (of_type == nothing_sym) null
    else if (of_type == null_sym) null
    else if (is_null(of_class))
      err(f, consym("undeclared class: ", of_type))
    else f.set_formal_of_class(of_class)
  };

  var unit_sym: Symbol = io.symbol("Unit");
  var bool_sym: Symbol = io.symbol("Boolean");
  var int_sym: Symbol = io.symbol("Int");
  var string_sym: Symbol = io.symbol("String");
  var ignore_sym: Symbol = io.symbol("_ignore");
  var error_type: Symbol = nothing_sym;

  def check_expr(e: Expression, env: Environment): Symbol = {
    e.set_of_type(e match {
      case n: Cno_expr => nothing_sym

      case v: Cvariable =>
        var vb: CoolNode = env.lookup_variable(v.get_name());
        e.set_binding(vb);
        if (is_null(vb)) {
          err(v, consym("undeclared variable: ", v.get_name()));
          error_type
        } else binding_type(vb)

      case a: Cassign =>
        var vtype: Symbol = any_sym;
        var name: Symbol = a.get_name();
        var b: CoolNode = env.lookup(a.get_name());
        if (is_null(b)) b = a.get_binding() else a.set_binding(b);
        b match {
          case null =>
            err(a, consym("undeclared variable in assignment ", name))
          case b: Case =>
            err(a, consym("cannot assign to branch variable ", name))
          case t: Class =>
            err(a, "cannot assign to 'this'")
          case f: Formal =>
            err(a, consym("cannot assign to formal ", name))
          case vb: CoolNode =>
            vtype = binding_type(vb)
        };
        env.check_type_leq(
          a,
          consym("assignment to ", name),
          check_expr(a.get_expr(), env),
          vtype
        );
        unit_sym

    case d: Cdispatch =>
      check_exprs(d.get_actuals(), env)
      val receiverSymbol = check_expr(d.get_expr(), env) match {
        case null => nothing_sym
        case s: Symbol =>
          features_env.getClassMethods(s) match {
            case Some(methods) =>
              methods.get(d.get_name()) match {
                case Some(m_untyped) =>
                  m_untyped match {
                    case m: Cmethod =>
                      d.set_mbinding(m)
                  }
                case None =>
                  err(d, f"there is no method with name: ${d.get_name()}")
              }
            case None =>
              err(d, f"there is no class for the undefined receiver type of dynamic dispatch: ${s}")
          }
          s
      }
      check_dispatch(d, receiverSymbol, d.get_name(), d.get_actuals())

      case d: Cstatic_dispatch =>
        check_exprs(d.get_actuals(), env)
        if (is_null(class_table.lookup_class(d.get_type_name())))
          err(d, consym("Undeclared class: ", d.get_type_name()))
        else {
          env.check_type_leq(
            d,
            "internal static dispatch",
            check_expr(d.get_expr(), env),
            d.get_type_name()
          )
          features_env.getClassMethods(d.get_type_name()) match {
            case Some(methods) =>
              methods.get(d.get_name()) match {
                case Some(m_untyped) =>
                  m_untyped match {
                    case m: Cmethod =>
                      d.set_mbinding(m)
                  }
                case None =>
                  err(d, f"there is no method with name: ${d.get_name()}")
              }
            case None =>
              err(d, f"there is no class for the undefined receiver type of static dispatch: ${d.get_type_name()}")
          }
        }
        check_dispatch(d, d.get_type_name(), d.get_name(), d.get_actuals())

      case c: Ccond =>
        check_bool(c, "predicate of 'if'", c.get_pred(), env);
        env.type_lub(
          check_expr(c.get_then_exp(), env),
          check_expr(c.get_else_exp(), env)
        )

      case l: Cloop =>
        check_bool(l, "predicate of 'while'", l.get_pred(), env);
        check_expr(l.get_body(), env);
        unit_sym

      case c: Ctypecase =>
        check_expr(c.get_expr(), env);
        var vt: Symbol = c.get_expr().get_of_type();
        var cs: Cases = c.get_cases();
        var rt: Symbol = nothing_sym;
        var ei: CasesEnumeration = cs.elements();
        while (ei.hasNext()) {
          ei.next() match {
            case ci: Cbranch =>
              var is: Symbol = ci.get_local_type();
              if (is == null_sym)
                if (symbol_name(ci.get_name()) == "null")
                  if (current_env.type_leq(is, vt)) ()
                  else
                    err(
                      ci,
                      consym("Case not possible for ", vt).concat(": 'Null'")
                    )
                else
                  err(ci, "'Null' cannot be explicitly checked for, use 'null'")
              else if (is == nothing_sym)
                err(ci, "'Nothing' is not a legal case")
              else
                class_table.lookup_class(is) match {
                  case null => err(ci, consym("Undeclared case class: ", is))
                  case cd: Cclass_decl =>
                    ci.set_case_of_class(cd);
                    if (current_env.type_leq(is, vt)) ()
                    else if (current_env.type_leq(vt, is)) ()
                    else
                      err(
                        ci,
                        consym(
                          consym("Case not possible for ", vt).concat(": "),
                          is
                        )
                      )
                };
              var done: Boolean = false;
              var ej: CasesEnumeration = cs.elements();
              while (!done) {
                ej.next() match {
                  case cj: Cbranch =>
                    if (ci == cj) done = true
                    else if (
                      if (is == null_sym) cj.get_local_type() == null_sym
                      else current_env.type_leq(is, cj.get_local_type())
                    ) {
                      err(ci, consym("Case already covered: ", is));
                      done = true
                    } else ()
                }
              };
              var newenv: Environment =
                new SingleEnvironment(ci.get_name(), ci, env);
              var ty: Symbol = check_expr(ci.get_expr(), newenv);
              ci.set_case_of_type(ty);
              rt = env.type_lub(rt, ty)
          }
        };
        rt

      case b: Cblock =>
        check_exprs(b.get_body(), env)

      case l: Clet =>
        var n: Symbol = l.get_identifier();
        var ty: Symbol = l.get_local_type();
        if (!is_null(env.lookup(n)))
          err(l, consym("local shadows existing variable ", n))
        else null;
        class_table.lookup_class(ty) match {
          case null =>
            if (ty == null_sym) ()
            else if (ty == nothing_sym) ()
            else err(l, consym("type of local is undeclared: ", ty))
          case cd: Cclass_decl =>
            l.set_of_class(cd)
        };
        env.check_type_leq(
          l,
          consym("initialization of ", n),
          check_expr(l.get_init(), env),
          ty
        );
        check_expr(l.get_body(), new SingleEnvironment(n, l, env))

      case op: Cadd =>
        check_binary(op, "+", op.get_e1(), op.get_e2(), env); int_sym

      case op: Csub =>
        check_binary(op, "-", op.get_e1(), op.get_e2(), env); int_sym

      case op: Cmul =>
        check_binary(op, "*", op.get_e1(), op.get_e2(), env); int_sym

      case op: Cdiv =>
        check_binary(op, "/", op.get_e1(), op.get_e2(), env); int_sym

      case op: Cneg =>
        check_int(op, "operand of '-'", op.get_e1(), env); int_sym

      case op: Clt =>
        check_binary(op, "<", op.get_e1(), op.get_e2(), env); bool_sym

      case op: Cleq =>
        check_binary(op, "<=", op.get_e1(), op.get_e2(), env); bool_sym

      case op: Ccomp =>
        check_bool(op, "operand of '!'", op.get_e1(), env); bool_sym

      case l: Cint_lit    => int_sym
      case l: Cbool_lit   => bool_sym
      case l: Cstring_lit => string_sym

      case a: Calloc =>
        var t: Symbol = a.get_type_name();
        if (t == bool_sym) err(a, "cannot instantiate value class Boolean")
        else if (t == int_sym) err(a, "cannot instantiate value class Int")
        else if (t == unit_sym) err(a, "cannot instantiate value class Unit")
        else if (t == any_sym) err(a, "cannot instantiate value class Any")
        else if (t == symbol_sym)
          err(a, "cannot instantiate value class Symbol")
        else
          class_table.lookup_class(t) match {
            case null =>
              err(a, consym("undeclared class for 'new': ", t))
            case cd: Cclass_decl =>
              a.set_of_class(cd)
          };
        t

      case u: Cunit => unit_sym
      case u: Cnil  => null_sym
    });
    e.get_of_type()
  };

  var last_type: Symbol = null;
  def check_exprs(es: Expressions, env: Environment): Symbol = {
    var saved: Environment = current_env;
    current_env = env;
    last_type = unit_sym;
    es.accept(this);
    current_env = saved;
    last_type
  };
  override def visit_Expressions_one(e: Expression): Any =
    last_type = check_expr(e, current_env);

  def check_binary(
      op: CoolNode,
      name: String,
      e1: Expression,
      e2: Expression,
      env: Environment
  ): Unit = {
    check_int(op, "left operand of '".concat(name).concat("'"), e1, env);
    check_int(op, "right operand of '".concat(name).concat("'"), e2, env)
  };

  def check_int(
      n: CoolNode,
      desc: String,
      e: Expression,
      env: Environment
  ): Unit = {
    val t = check_expr(e, env)
    if (!(check_expr(e, env) == int_sym))
      err(n, desc.concat(f" must be 'Int' ${t}"))
    else ();
  }

  def check_bool(
      n: CoolNode,
      desc: String,
      e: Expression,
      env: Environment
  ): Unit = {
    val t = check_expr(e, env)
    if (!(t == bool_sym))
      err(n, desc.concat(f" must be 'Boolean' but its ${t}"))
    else ();
  }

  def binding_type(c: CoolNode): Symbol =
    c match {
      case null            => null
      case cd: Cclass_decl => cd.get_name()
      case a: Cattr        => a.get_of_type()
      case f: Cformal      => f.get_of_type()
      case l: Clet         => l.get_local_type()
      case b: Cbranch      => b.get_local_type()
      case m: Cmethod      => m.get_return_type()
      case _ => {
        println(f"binding_type bad: ${c.getClass}")

        any_sym
      }
    };

  def check_dispatch(
      e: Expression,
      tn: Symbol,
      mn: Symbol,
      es: Expressions
  ): Symbol = {
    var m: Cmethod = e.get_mbinding();
    if (!is_null(m)) {
      var fs: Formals = m.get_formals();
      var n: Int = fs.size();
      if (!(es.size() == n))
        err(
          e,
          "Wrong number of parameters ("
            .concat(es.size().toString())
            .concat(") to ")
            .concat(mn.toString())
            .concat("', expected ")
            .concat(n.toString())
        )
      else {
        var fe: FormalsEnumeration = fs.elements();
        var ee: ExpressionsEnumeration = es.elements();
        var i: Int = 0;
        while (fe.hasNext()) {
          i = i + 1;
          fe.next() match {
            case f: Cformal =>
              current_env.check_type_leq(
                e,
                "parameter #".concat(i.toString()),
                ee.next().get_of_type(),
                f.get_of_type()
              )
          }
        }
      };
      m.get_return_type()
    } else {
      println(f"dispatch to undefined method: ${mn}")
      error_type
    }
  };
  // #)
}

class SemantErrors(var options: CoolOptions) extends IO() {
  var num_errors: Int = 0;

  def error(cd: Cclass_decl, loc: CoolNode, s: String): Unit = {
    if (options.get_semant_debug())
      out_any(loc)
    else {
      if (is_null(cd)) out("<unknown>")
      else out(symbol_name(cd.get_filename()));
      out(":").out_any(loc.get_line_number())
    };
    out(": ").out(s).out("\n");
    num_errors = num_errors + 1
  };

  def has_errors(): Boolean = 0 < num_errors;
}

/** This class represents a partially implemented interface used with the
  * DECORATOR design pattern. Please read up on this design pattern.
  */
class Environment() extends IO() {
  def err(loc: CoolNode, message: String): Unit = abort("abstract!");

  def error(cd: Cclass_decl, loc: CoolNode, message: String): Unit =
    abort("abstract!");

  def lookup(key: Symbol): CoolNode = abort("abstract");

  def lookup_variable(key: Symbol): CoolNode = abort("abstract");

  def type_leq(t1: Symbol, t2: Symbol): Boolean = type_lub(t1, t2) == t2;

  def check_type_leq(n: CoolNode, s: String, t1: Symbol, t2: Symbol): Unit =
    if (!type_leq(t1, t2))
      err(n, "Type mismatch at ".concat(s).concat(f" !type_leq(${t1}, ${t2})"))
    else ();

  def type_lub(t1: Symbol, t2: Symbol): Symbol = abort("abstract!");
}

// #(
class RootEnvironment(var class_table: ClassTable, var errors: SemantErrors)
    extends Environment() {
  override def lookup(key: Symbol): CoolNode = null;
  override def lookup_variable(key: Symbol): CoolNode = null;

  override def error(cd: Cclass_decl, loc: CoolNode, message: String): Unit =
    errors.error(cd, loc, message);
  override def err(loc: CoolNode, message: String): Unit =
    error(null, loc, message);

  var any_sym: Symbol = symbol("Any");
  var null_sym: Symbol = symbol("Null");
  var nothing_sym: Symbol = symbol("Nothing");
  var bool_sym: Symbol = symbol("Boolean");
  var int_sym: Symbol = symbol("Int");
  var unit_sym: Symbol = symbol("Unit");


  def createClassList(sym: Symbol): Seq[Symbol] = {
    val parent = class_table.parentMap.getOrElse(sym, any_sym);

    if(parent == any_sym) {
      List(sym)
    } else {
      List(sym) ++ createClassList(parent)
    }
  }

  override def type_lub(t1: Symbol, t2: Symbol): Symbol = {
    if (t1 == t2 || t1 == nothing_sym || t2 == nothing_sym) {
      return t2
    }

    if (t1 == bool_sym || t2 == bool_sym ||
        t1 == int_sym || t2 == int_sym ||
        t1 == unit_sym || t2 == unit_sym) {
      return any_sym
    }

    if (t1 == null_sym) {
      return t2
    } else if(t2 == null_sym) {
      return t1
    }

    val t1List = createClassList(t1)
    val t2List = createClassList(t2)

    for (c <- t1List) {
      if (t2List.contains(c)) {
        return c
      }
    }
    any_sym
  }
}
// #)

/** A abstract Decorator (q.v.) for a nested contour. This class is extended by
  * concrete decorator classes. Compare to java.io.FilterOutputStream.
  */
class NestedEnvironment(var outer: Environment) extends Environment() {
  override def error(cd: Cclass_decl, loc: CoolNode, message: String): Unit =
    outer.error(cd, loc, message);

  override def err(loc: CoolNode, message: String): Unit =
    outer.err(loc, message);

  override def lookup(key: Symbol): CoolNode = outer.lookup(key);
  override def lookup_variable(key: Symbol): CoolNode = outer.lookup_variable(key);

  override def type_lub(t1: Symbol, t2: Symbol): Symbol =
    outer.type_lub(t1, t2);
}

// #(
class TableEnvironment(var outere: Environment)
    extends NestedEnvironment(outere) {
  var table: Hashtable = new Hashtable();

  def probe(key: Symbol): CoolNode = {
    table.get(key) match {
      case null         => null
      case vb: CoolNode => vb
    }
  };

  override def lookup(key: Symbol): CoolNode = {
    table.get(key) match {
      case null         => super.lookup(key)
      case vb: CoolNode => vb
    }
  };

  override def lookup_variable(key: Symbol): CoolNode = {
    table.get(key) match {
      case null         => super.lookup_variable(key)
      case vb: CoolNode => vb
    }
  };

  def add(name: Symbol, vb: CoolNode): Unit = {
    table.put(name, vb); ()
  };
}
// #)

/** A concrete decorator for a contour with a single declaration.
  */
class SingleEnvironment(
    var name: Symbol,
    var node: CoolNode,
    var outere: Environment
) extends NestedEnvironment(outere) {
  override def lookup(key: Symbol): CoolNode =
    if (key == name) node else super.lookup(key);

  override def lookup_variable(key: Symbol): CoolNode = {
    var is_method = node match {
        case m: Cmethod => true
        case _ => false
    };

    if (key == name && !is_method)
      node
    else
      super.lookup_variable(key);
  };
}

// #(
class ClassEnvironment(var class_decl: Cclass_decl, var superenv: Environment)
    extends TableEnvironment(superenv) {
  { add(symbol("this"), class_decl) };

  override def err(loc: CoolNode, message: String): Unit =
    superenv.error(class_decl, loc, message);
}
// #)
// PA4: Define new environment classes
//      The solution has three other environment classes.
//      Only two are used in the PA4 solution.

class ClassTable(var classes: Classes, var errors: SemantErrors)
    extends CoolVisitor() {
  var table: Hashtable = new Hashtable();

  import Constants._;

  var classMap: Map[Symbol, Cclass_decl] = Map();
  var parentMap: Map[Symbol, Symbol] = Map();

  { classes.accept(this) } ;

  override def visit_Classes_one(cl: Class): Any = cl.accept(this);
  // #(
  override def visit_class_decl(
      n: Cclass_decl,
      name: Symbol,
      parent: Symbol,
      features: Features,
      filename: Symbol
  ): Any = {
    classMap += (name -> n)
    parentMap += (name -> parent)

    if (is_null(table.get(name)))
      table.put(name, new ClassDeclInfo(n, errors))
    else null;
  }
  // #)
  // Use Visitor to insert every class into the table.
  // For PA4, the value can be the class_decl node itself
  // For PA5, you may want to put in a structure that
  //          can be used as a node in the inheritance graph.

  // #(
  var no_info: ClassInfo = new ClassInfo();

  def lookup(sym: Symbol): ClassInfo = {
    if (is_null(sym)) no_info
    else
      table.get(sym) match {
        case null          => no_info
        case ci: ClassInfo => ci
      }
  };

  def lookup_class(sym: Symbol): Cclass_decl = lookup(sym).get_class_decl();

  def get_info(c: Class): ClassInfo = {
    c match {
      case null            => no_info
      case cd: Cclass_decl => lookup(cd.get_name())
    }
  };
  /* #)
  def lookup_class(sym : Symbol) : Cclass_decl = abort("TODO");
  ## */
}

// #(
class ClassInfo() extends IO() {
  def get_class_decl(): Cclass_decl = null;
  // Add other "abstract" methods here.
}

class ClassDeclInfo(var class_decl: Cclass_decl, var errors: SemantErrors)
    extends ClassInfo() {
  override def get_class_decl(): Cclass_decl = class_decl;

}

// // This is how I would create my Method and Attribute Tables
// class MethodTable(var features : Features, var errors : SemantErrors) 
// extends CoolVisitor() 
// {
//   var mtable : Hashtable = new Hashtable();

//   { features.accept(this) } ;

//   override def visit_Features_one(f : Feature) : Any = f.accept(this);
//   // #(
//   override def visit_method(n : Cmethod, overridep:Boolean,name:Symbol,formals:Formals,return_type:Symbol,expr:Expression) : Any =
//     if (is_null(table.get(name)))
//       mtable.put(name,new MethodDecInfo(n,errors))
//     else null;
//   // #)
//   // Use Visitor to insert every class into the table.
//   // For PA4, the value can be the method node itself
//   // For PA5, you may want to put in a structure that
//   //          can be used as a node in the inheritance graph.

//   // #(
//   var no_info : MethodInfo = new MethodInfo();

//   def lookup(sym : Symbol) : MethodInfo = {
//     if (is_null(sym)) no_info
//     else mtable.get(sym) match {
//       case null => no_info
//       case f:MethodInfo => f
//     }
//   };

//   def lookup_method(sym : Symbol) : Cmethod = lookup(sym).get_method();

//   def get_info(f:Feature) : MethodInfo = {
//     f match {
//       case null => no_info
//       case m:Cmethod => lookup(m.get_name())
//     }
//   };
//   /* #)
//   def lookup_method(sym : Symbol) : Cmethod = abort("TODO");
//   ## */
// }

// // #(
// class MethodInfo() extends IO()
// {
//   def get_method() : Cmethod = null;
//   // Add other "abstract" methods here.
// }

// class MethodDecInfo(var method : Cmethod, var errors : SemantErrors) 
// extends MethodInfo() {
//   override def get_method : Cmethod = method;
// }

// // Attr Table
// class AttrTable(var features : Features, var errors : SemantErrors) 
// extends CoolVisitor() 
// {
//   var atable : Hashtable = new Hashtable();

//   { features.accept(this) } ;

//   override def visit_Features_one(f : Feature) : Any = f.accept(this);
//   // #(
//   override def visit_attr(n : Cattr, name:Symbol,of_type:Symbol) : Any =
//     if (is_null(table.get(name)))
//       atable.put(name,new AttrDecInfo(n,errors))
//     else null;
//   // #)
//   // Use Visitor to insert every attr into the table.
//   // For PA4, the value can be the attr node itself
//   // For PA5, you may want to put in a structure that
//   //          can be used as a node in the inheritance graph.

//   // #(
//   var no_info : AttrInfo = new AttrInfo();

//   def lookup(sym : Symbol) : AttrInfo = {
//     if (is_null(sym)) no_info
//     else atable.get(sym) match {
//       case null => no_info
//       case a:AttrInfo => f
//     }
//   };

//   def lookup_attr(sym : Symbol) : Cattr = lookup(sym).get_method();

//   def get_info(f:Feature) : AttrInfo = {
//     f match {
//       case null => no_info
//       case attr:Cattr => lookup(attr.get_name())
//     }
//   };
//   /* #)
//   def lookup_attr(sym : Symbol) : Cattr = abort("TODO");
//   ## */
// }

// // #(
// class AttrInfo() extends IO()
// {
//   def get_attr() : Cmethod = null;
//   // Add other "abstract" methods here.
// }

// class AttrDecInfo(var attr : Cattr, var errors : SemantErrors) 
// extends AttrInfo() {
//   override def get_attr : Cattr = attr;
// }

// Many more helper classes for PA5:
//   - nodes for inheritance graph
//   - method tables
//   - helpers to create attr/method tables
//#)
