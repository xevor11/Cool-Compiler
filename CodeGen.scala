import scala.collection.immutable.List
import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

class CodeGen(var program : Program, var options : CoolOptions, var output : IO)
extends Generator()
{
  override def run() : Unit = {
    program.accept(this); ()
  };

  var emitter : Emitter = new Emitter(output,options.get_enable_gc());
  var classInfoList = ListBuffer[ClassCodeInfo]()
  var classCodeInfo : ClassCodeInfo = null;
  var code_info_table : CodeInfoTable = null;

  // #(
  var any_class_info : ClassCodeInfo = null;
  var int_class_info : ClassCodeInfo = null;
  var unit_class_info : ClassCodeInfo = null;
  var boolean_class_info : ClassCodeInfo = null;
  var string_class_info : ClassCodeInfo = null;

  var string_table : StringTable = null;
  var int_table : IntTable = null;

  var null_sym : Symbol = symbol("Null");
  var tempCalc = new TempCalculator(code_info_table)
  var label = ""
  var label_counter = ""
  var curr = 0

  // #)

  override def visit_program(the_node:Cprogram,classes:Classes) : Any = {
    code_info_table = new CodeInfoTable(options,classes);    
    // #(
    any_class_info = code_info_table.get_class_info(program.get_any_class());
    int_class_info = code_info_table.get_class_info(program.get_int_class());
    unit_class_info = code_info_table.get_class_info(program.get_unit_class());
    boolean_class_info =
      code_info_table.get_class_info(program.get_boolean_class());
    string_class_info =
      code_info_table.get_class_info(program.get_string_class());
    var empty : ArrayAny = new ArrayAny(0);
    any_class_info.set_tags(0,empty,empty);
    // #)
    // PA6: set offsets

    if (options.get_cgen_debug()) {
      new PrintCodeInfo(code_info_table).run(the_node)
    } else {};

    emit_boilerplate();
    
    // #(
    int_table = new IntTable(int_class_info.get_class_tag());
    string_table = new StringTable(string_class_info.get_class_tag(),int_table);
    program.accept(new FillLitTables(string_table,int_table));

    // emit all literals:
    var unit_tag : Int = unit_class_info.get_class_tag();
    emitter.obj_header(emitter.s_UNITLIT(),"Unit",unit_tag,0);
    var boolean_tag : Int = boolean_class_info.get_class_tag();
    emitter.obj_header(emitter.s_FALSE(),"Boolean",boolean_tag,1);
    emitter.word(0);
    emitter.obj_header(emitter.s_TRUE(),"Boolean",boolean_tag,1);
    emitter.word(1);
    int_table.emit(emitter);
    string_table.emit(emitter);

    // class name table: (ordered by class tag)
    emitter.label_def(emitter.s_CLASSNAMETAB());
    any_class_info.emit_classname_table(emitter,string_table);

    // all prototype objects and dispatch tables
    any_class_info.emit(emitter);
    // #)
    // PA6: control all the setting of tags, generation of
    // literals, generation of prototype objects, generation of 
    // class name table.

    // all methods
    emitter.text();
    classes.accept(this);
    // needed to make spim happy
    // For some reason, this can't be put in cool-runtime.s
    emitter.label_def("main");
    emitter.opc("jr").opn(emitter.s_RA()).endl(program);
    emitter.endl(program);

    emitter.data();
    emitter.global_def(emitter.s_HEAPSTART())
  };

  def emit_boilerplate() : Unit = {
    emitter.global(emitter.s_CLASSNAMETAB());
    emitter.global("Main".concat(emitter.s_PROTOBJ()));
    emitter.global("Int".concat(emitter.s_PROTOBJ()));
    emitter.global("Boolean".concat(emitter.s_PROTOBJ()));
    emitter.global("String".concat(emitter.s_PROTOBJ()));
    emitter.global("Symbol".concat(emitter.s_PROTOBJ()));
    emitter.global(emitter.s_UNITLIT());
    emitter.global(emitter.s_FALSE());
    emitter.global(emitter.s_TRUE());
    ()
  };

  override def visit_Classes_one(node:Class) : Any = node.accept(this);
  override def visit_class_decl(cd:Cclass_decl,name:Symbol,parent:Symbol,
                                features:Features,filename:Symbol) : Any = {
    current_class = cd;
    classCodeInfo = code_info_table.get_class_info(current_class)
    features.accept(this)
  };
  var current_class : Cclass_decl = null;

  override def visit_Features_one(f : Feature) : Any = f.accept(this);

  override def visit_attr(n:Cattr,name:Symbol,of_type:Symbol) : Any = null;

  override def visit_method(the_node:Cmethod,overridep:Boolean,name:Symbol,formals:Formals,return_type:Symbol,body:Expression) : Any = {
    //#(
    // PA7: compute number of temps needs
    // Assign offsets to formals
    var arr : ArrayAny = classCodeInfo.method_array
    var i = 0
    var n = arr.length();
    while (i < n) {
      var m : Cmethod = arr.get(i) match { case m:Cmethod =>  m};
      var o : Cclass_decl = m.get_owner();
      if(m.get_name() == name) {
        emitter.s_METHOD(o.get_name(),m.get_name());
      }
      i = i + 1
    };
    val numberOfFormals = formals.size()
    var currents = tempCalc.get_max()
    // If not native (no_expr),
    //   (1) emit prologue
    emitter.prologue(currents)
    body.accept(this)
    //   (3) emit epilogue
    emitter.epilogue(currents, numberOfFormals)
    tempCalc.set_max(currents)
    curr = currents
    //#)
  }

  override def visit_assign(the_node:Cassign,name:Symbol,expr:Expression) {
    expr.accept(this)
    emitter.opc("sw").opn(emitter.s_ACC()).offset(emitter.s_SP(), 4).endl(the_node)
  }

  override def visit_static_dispatch(the_node:Cstatic_dispatch,expr:Expression,type_name:Symbol,name:Symbol,actuals:Expressions) : Any = {
    expr.accept(this);
    actuals.accept(this);
    var m : Cmethod = the_node.get_mbinding()
    var cd : Cclass_decl = m.get_owner()
    var method_name = emitter.s_METHOD(cd.get_name(), m.get_name())
    var t : Int = code_info_table.get_offset_info(the_node)
    spill(the_node.get_expr(), t)
    get_actuals(the_node.get_actuals())
    unspill(the_node, t, emitter.s_ACC())
    emitter.opc("jal").opn(method_name).endl(the_node)
  }

  override def visit_dispatch(the_node:Cdispatch,expr:Expression,name:Symbol,actuals:Expressions) : Any = {
    expr.accept(this)
    actuals.accept(this)
    var m : Cmethod = the_node.get_mbinding()
    var t : Int = code_info_table.get_offset_info(the_node)
    spill(the_node.get_expr(), t)
    get_actuals(the_node.get_actuals())
    unspill(the_node, t, emitter.s_ACC())
    var label : String = gen_label()
    emitter.opc("bnez").opn(emitter.s_ACC()).opn(label).endl(the_node)
    emitter.error_abort(emitter.s_DISPATCHABORT(), string_table.get(current_class.get_filename()), the_node.get_line_number())
    emitter.label_def(label)
    emitter.opc("lw").opc(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_DISPTABOFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), code_info_table.get_offset_info(m)).endl(the_node)
    emitter.opc("jalr").opn(emitter.s_T1()).endl(the_node)
  }

  override def visit_cond(the_node: Ccond, pred: Expression, then_exp: Expression, else_exp: Expression): Any = {
    pred.accept(this)
    val thenLabel = gen_label()
    val elseLabel = gen_label()
    emitter.opc("beqz").opn(emitter.s_ACC()).opn(elseLabel).endl(the_node) 
    emitter.label_def(thenLabel)
    then_exp.accept(this)
    val endLabel = gen_label()
    emitter.opc("j").opn(endLabel).endl(the_node)
    emitter.label_def(elseLabel)
    else_exp.accept(this)
    emitter.label_def(endLabel)
  }

  override def visit_loop(the_node: Cloop, pred: Expression, body: Expression): Any = {
    val startLabel = gen_label()
    val endLabel = gen_label()
    emitter.label_def(startLabel)
    pred.accept(this)
    emitter.opc("beqz").opn(emitter.s_ACC()).opn(endLabel).endl(the_node) 
    body.accept(this)
    emitter.opc("j").opn(startLabel).endl(the_node)
    emitter.label_def(endLabel)
  }

  override def visit_branch(the_node: Cbranch, name: Symbol, local_type: Symbol, expr: Expression): Any = {
    emitter.label_def(name.toString())
    expr.accept(this)
    emitter.opc("move").opn(emitter.s_ACC()).opn(emitter.s_T1()).endl(the_node)
  }

  override def visit_typecase(the_node:Ctypecase,expr:Expression,cases:Cases) : Any = {
    expr.accept(this)
    cases.accept(this)
    var it = cases.elements();
    while (it.hasNext()) {
      it.next() match {
        case b: Cbranch => if (b.local_type == null) b.set_local_type(null_sym) else ()
      }
    }
  }

  override def visit_block(the_node:Cblock,body:Expressions) {
    body.accept(this)
  }

  override def visit_let(the_node: Clet, identifier: Symbol, local_type: Symbol, init: Expression, body: Expression): Any = {
    init.accept(this)
    emitter.opc("sw").opn(emitter.s_ACC()).offset(emitter.s_FP(), -(curr + emitter.i_ATTROFFSET())).endl(the_node)
    emitter.prologue(curr + 1)
    body.accept(this)
    emitter.epilogue(curr + 1, 1)
  }

  override def visit_add(the_node:Cadd,e1:Expression,e2:Expression) = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("add").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_sub(the_node:Csub,e1:Expression,e2:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("sub").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_mul(the_node:Cmul,e1:Expression,e2:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("mul").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_div(the_node:Cdiv,e1:Expression,e2:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("div").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_neg(the_node:Cneg,e1:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    emitter.opc("jal").opn(emitter.s_ANYCLONE())
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("Anyg").opn(emitter.s_T1).opn(emitter.s_T1()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_lt(the_node:Clt,e1:Expression,e2:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("blt").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC, emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_leq(the_node:Cleq,e1:Expression,e2:Expression) : Any = {
    e1.accept(this)
    spill(e1, code_info_table.get_offset_info(the_node))
    e2.accept(this)
    emitter.opc("jal").opn(emitter.s_ANYCLONE()).endl(the_node)
    unspill(the_node, code_info_table.get_offset_info(the_node), emitter.s_T1())
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_T2()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("ble").opn(emitter.s_T1()).opn(emitter.s_T1()).opn(emitter.s_T2()).endl(the_node)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_comp(the_node:Ccomp,e1:Expression) {
    e1.accept(this)
    emitter.opc("lw").opn(emitter.s_T1()).offset(emitter.s_T1(), emitter.i_ATTROFFSET()).endl(the_node)
    emitter.opc("la").opn(emitter.s_ACC()).opn(emitter.s_TRUE())
    var label = gen_label()
    emitter.opc("beqz").opn(emitter.s_T1()).opn(label)
    emitter.opc("sw").opn(emitter.s_T1()).offset(emitter.s_ACC(), emitter.i_ATTROFFSET()).endl(the_node)
  }

  override def visit_int_lit(the_node: Cint_lit, token: Symbol): Any = {
    val sym = the_node.get_token().toString()
    val intEnum = int_table.table.elements()
    while (intEnum.hasNext()) {
      val entry = intEnum.next()
      entry match {
        case pair: Pair =>
          val entryKey = pair.first().asInstanceOf[Symbol]
          val entryValue = pair.second().asInstanceOf[Int]
          if (sym == symbol_name(entryKey)) {
            emitter.opc("la").opn(emitter.s_ACC()).opn(entryValue.toString).endl(the_node)
          }
        case _ => 
      }
    }
  }

  override def visit_bool_lit(the_node:Cbool_lit,value:Boolean) : Any = {
    val boolValue = if (the_node.get_value()) "1" else "0"
    emitter.opc("la").opn(emitter.s_ACC()).opn(boolValue).endl(the_node)
  }

  override def visit_string_lit(the_node: Cstring_lit, token: Symbol): Any = {
    val sym = the_node.get_token().toString()
    val stringEnum = string_table.table.elements()
    while (stringEnum.hasNext()) {
      val entry = stringEnum.next()
      entry match {
        case pair: Pair =>
          val entryKey = pair.first().asInstanceOf[Symbol]
          val entryValue = pair.second()
          if (sym == symbol_name(entryKey)) {
            emitter.opc("la").opn(emitter.s_ACC()).opn(entryValue.toString).endl(the_node)
          }
        case _ =>
      }
    }
  }

  override def visit_nil(the_node:Cnil) : Any = {
    emitter.opc("li").opn(emitter.s_ACC()).opn(0.toString).endl(the_node)
  }

  override def visit_unit(the_node:Cunit) : Any = {
    emitter.opc("la").opn(emitter.s_ACC()).opn(emitter.s_UNITLIT()).endl(the_node)
  }

  override def visit_no_expr(the_node:Cno_expr) : Any = {
  }

  override def visit_variable(the_node:Cvariable,name:Symbol) : Any = {
    var this_sym : Symbol = symbol("this");
    if(the_node.get_name() == this_sym)
    emitter.opc("move").opn(emitter.s_ACC()).opn(emitter.s_SELF()).endl(the_node)
    emitter.opc("lw").opn(emitter.s_ACC()).offset(emitter.s_SELF(), emitter.i_ATTROFFSET()).endl(the_node)
  };

  override def visit_Expressions_one(node:Expression) : Any = {
    var current = tempCalc.get_max()
    // Iterating string tables and finding correct label
    tempCalc.set_max(current)
    curr = current
  }

  def gen_label(): String ={
    var label = s"L${label_counter}"
    label_counter = label_counter + 1; 
    label_counter
  }

  def spill(e: Expression, offset: Int): Unit ={
    emitter.opc("sw").opn(emitter.s_ACC()).opn(tempCalc.current_offset.toString).endl(e);
  }

  def unspill(e: Expression, offset: Int, to_register: String): Unit = {
    emitter.opc("lw").opn(to_register).opn(tempCalc.current_offset.toString).endl(e);
  }

  def get_actuals(expr: Expressions): Unit = {
    val enum = expr.elements()
    while (enum.hasNext()) {
      val expression = enum.next()
    }
  }
}

/** A class to handle formatting the assembly file.
 */
class Emitter(var output : IO, var enable_gc : Boolean) extends IO()
{
  // { new IO().out("enable_gc = ").out_any(enable_gc).out("\n") };
  override def out(s : String) : IO = output.out(s);
  var code_info_table_emitter : CodeInfoTable = null;
  var tempCalcEmitter = new TempCalculator(code_info_table_emitter)
  // conventions
  def s_CLASSNAMETAB() : String = "class_nameTab";
  def s_CLASSOBJTAB() : String = "class_objTab";
  def s_HEAPSTART() : String = "heap_start";
  def s_DISPTAB() : String = "_dispTab";
  def s_PROTOBJ() : String = "_protObj";
  def s_METHODSEP() : String = ".";
  def s_ANYCLONE() : String = "Any.clone";
  def s_ERRORABORT() : String = "_error_abort";

  def s_METHOD(cn : Symbol, mn : Symbol) : String =
    symbol_name(cn).concat(s_METHODSEP()).concat(symbol_name(mn));
  
  def s_INTLIT(offset : Int) : String = 
    "int_lit".concat(offset.toString());
  def s_STRINGLIT(offset : Int) : String = 
    "string_lit".concat(offset.toString());
  def s_BOOLEANLIT(value : Boolean) : String =
    if (value) "boolean_lit1" else "boolean_lit0";
  def s_UNITLIT() : String = "unit_lit";

  def s_FALSE() : String = s_BOOLEANLIT(false);
  def s_TRUE() : String = s_BOOLEANLIT(true);

  def s_DISPATCHABORT() : String = "_dispatch_abort";
  def s_DIVABORT() : String = "_divide_abort";
  def s_CASEABORT() : String = "_case_abort";

  def i_WORDSIZE() : Int = 4;
  def i_TAGOFFSET() : Int = 0;
  def i_SIZEOFFSET() : Int = 1;
  def i_DISPTABOFFSET() : Int = 2;
  def i_ATTROFFSET() : Int = 3;
  def i_METHODOFFSET() : Int = 0;

  def s_ZERO() : String = "$zero";
  def s_ACC() : String = "$a0";
  def s_AUX() : String = "$a1"; // for some primitive functions
  def s_SELF() : String = "$s0";
  def s_T1() : String = "$t1";
  def s_T2() : String = "$t2";
  def s_T3() : String = "$t3";
  def s_SP() : String = "$sp";
  def s_FP() : String = "$fp";
  def s_RA() : String = "$ra";

  { out("\t.data\n") };
  var mode : Int = 1; // 0 = text, 1 = data, 2 = in ascii

  def gen_offset(s : String, o : Int) : String =
    (o*i_WORDSIZE()).toString().concat("(").concat(s).concat(")");

  def opc(s : String) : Emitter = {out("\t").out(s).out("\t");this};
  def opn(s : String) : Emitter = {out(" ").out(s); this};
  def offset(s : String, o : Int) : Emitter = opn(gen_offset(s,o));

  var last_line : Int = 0;
  def endl(n : CoolNode) : Unit = {
    var l : Int = n.get_line_number();
    if (l == last_line) ()
    else if (l == 0) ()
    else { out("\t# line ").out_any(l); last_line = l };
    out("\n"); ()
  };

  def set_mode(n : Int) : Unit = {
    while (!(mode == n)) {
      if (mode == 2) {
	out("\"\n");
	mode = 1
      } else if (mode == 0) {
	out("\t.data\n");
	mode = 1
      } else if (n == 0) {
	out("\t.text\n");
	mode = 0
      } else {
	opc(".ascii").out("\"");
	mode = 2
      }
    }
  };

  def text() : Unit = set_mode(0);
  def data() : Unit = set_mode(1);
  def ascii() : Unit = set_mode(2);

  def char(ch : String) : Unit = {
    ascii();
    out(ch);
    ()
  };

  def byte(i : Int) : Unit = {
    data();
    opc(".byte").out_any(i).out("\n");
    ()
  };

  def align() : Unit = { 
    data(); 
    opc(".align").out_any(2).out("\n"); 
    ()
  };

  def global(name : String) : Any = opc(".globl").out(name).out("\n");
  def label_def(name : String) : Any = out(name).out(":\n");
  def global_def(name : String) : Any = { global(name); label_def(name) };

  def word(x : Any) : Any = out("\t.word\t").out(x.toString()).out("\n");

  {
    data();
    global_def("_MemMgr_INITIALIZER");
    word(if (enable_gc) "_GenGC_Init" else "_NoGC_Init");
    global_def("_MemMgr_COLLECTOR");
    word(if (enable_gc) "_GenGC_Collect" else "_NoGC_Collect");
    global_def("_MemMgr_TEST");
    word(0)
  };

  /** Generate code to record file and line numebr and then jump
   * to the appropriate error reporting routine in the runtime.
   */
  // An example of a helper method.  
  // The solution uses this whenever it needs to handle an "abort"
  def error_abort(target: String, fi : Int, li : Int) : Unit = {
    opc("la").opn(s_AUX()).opn(s_STRINGLIT(fi)).out("\n");
    opc("li").opn(s_T1()).opn(li.toString()).out("\n");
    opc("j").opn(target).out("\n");
    ()
  };

  // CS 654 students don't need to call this method
  // Only use if garbage collection is enabled.
  def gc_assign(offset : Int) : Unit = {
    opc("la").opn(s_AUX()).offset(s_SELF(),offset+i_ATTROFFSET()).out("\n");
    opc("jal").opn("_GenGC_Assign").out("\n");
    ()
  };

  // #(
  def obj_header(label : String, cn : String, tag : Int, size : Int) : Unit = {
    data();
    word(-1);
    label_def(label);
    word(tag);
    word(size+i_ATTROFFSET());
    word(cn.concat(s_DISPTAB()));
    ()
  };
  // #)
  // PA6: The solution defines a method for generating an object header

  //## PA7: The solution defines methods to generate prologue and epilogue
  def prologue(temps: Int): Unit = {
    // TODO: complete the prologue method
    // Use the handout8 of cs654 and this method is need for PA7 and PA8
    if(enable_gc == true) {
        opc("addiu").opn(s_SP()).opn(s_SP()).opn(((temps + 3) * i_WORDSIZE()).toString).out("\n")
        opc("sw").opn(s_FP()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_SELF()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_FP()).offset(s_SP(), temps + 3).out("\n")
        opc("addiu").opn(s_SP()).opn(s_SP()).opn(temps.toString()).out("\n")
        opc("move").opn(s_SELF()).opn(s_ACC())
        opc("sw").opn(s_ZERO()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_ZERO()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_ZERO()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_ZERO()).offset(s_SP(), temps + 3).out("\n")
    } else {
        opc("addiu").opn(s_SP()).opn(s_SP()).opn(temps.toString()).out("\n")
        opc("sw").opn(s_FP()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_SP()).offset(s_SP(), temps + 3).out("\n")
        opc("sw").opn(s_RA()).offset(s_SP(), temps + 3).out("\n")
        opc("addiu").opn(s_FP()).opn(s_SP()).opn(temps.toString()).out("\n")
        opc("move").opn(s_SELF()).opn(s_ACC()).out("\n")
    }
  }

  def epilogue(temps: Int, formals: Int): Unit = {
    // TODO: complete the epilogue method
    // Use the handout8 of cs654 and this method is need for PA7 and PA8
    if(enable_gc == true) {
      opc("lw").opn(s_FP()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("lw").opn(s_SELF()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("lw").opn(s_RA()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("addiu").opn(s_SP()).opn(s_SP()).opn(temps.toString()).out("\n")
      opc("jr").opn(s_RA()).out("\n")
    } else {
      opc("lw").opn(s_FP()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("lw").opn(s_SELF()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("lw").opn(s_RA()).offset(s_SP(), temps + 3 + formals).out("\n")
      opc("addiu").opn(s_SP()).opn(s_SP()).opn(temps.toString()).out("\n")
      opc("jr").opn(s_RA()).out("\n")
    }
  }
}

class CodeInfoTable(var options : CoolOptions, var classes : Classes) 
extends CoolVisitor()
{
  var table : ArrayAny = new ArrayAny(100);
  
  def get_raw_info(n : CoolNode) : Any =
    if (table.length() <= n.get_id()) null else table.get(n.get_id());

  def set_info(n : CoolNode, v : Any) : Unit = {
    while (table.length() <= n.get_id()) table = table.resize(table.length()*2);
    table.set(n.get_id(),v); ()
  };

  def get_offset_info(n : CoolNode) : Int =
    get_raw_info(n) match { case i:Int => i };

  // #(
  def get_class_info(n : Class) : ClassCodeInfo =
    n match {
      case null => null
      case cd:Cclass_decl =>
	get_raw_info(n) match { 
	  case null => 
	    var ci : ClassCodeInfo = new ClassCodeInfo(cd,this);
	    set_info(n,ci); ci
	  case ci:ClassCodeInfo => ci 
	}
    };

  def get_class_tag(n : Class) : Int = get_class_info(n).get_class_tag();

  { 
    classes.accept(this)
  };

  override def visit_Classes_one(node:Class) : Any = {
    var ci : ClassCodeInfo = get_class_info(node);
    var si : ClassCodeInfo = get_class_info(node.get_superclass());
    if (is_null(si)) null
    else si.add_child(ci)
  };
  // #) 
  // TODO
  // def get_class_tag(node:Class) : Int = ...

}

class PrintCodeInfo(var code_info_table : CodeInfoTable) extends CoolTreeVisitor() {
  def run(p : Program) : Any = {
    p.accept(this)
  };

  override def visit_class_decl(the_node:Cclass_decl,name:Symbol,parent:Symbol,features:Features,filename:Symbol) : Any = {
    out("class tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_class_tag(the_node));
    out("\n");
    super.visit_class_decl(the_node,name,parent,features,filename)
  };

  override def visit_method(m:Cmethod,o:Boolean,name:Symbol,fs:Formals,
                            rtype:Symbol,expr:Expression) : Any = {
    out("  method tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_offset_info(m));
    out("\n");
    super.visit_method(m,o,name,fs,rtype,expr)
  };

  override def visit_attr(a:Cattr,name:Symbol,of_type:Symbol) : Any = {
    out("  attr tag for ").out(symbol_name(name)).out(" = ");
    out_any(code_info_table.get_offset_info(a));
    out("\n");
    super.visit_attr(a,name,of_type)
  };
}

// #(
class ClassCodeInfo(var class_decl : Cclass_decl, var table : CodeInfoTable) 
extends CoolVisitor()
{
  def get_class_decl() : Cclass_decl = class_decl;

  // graph connections
  var first_child : ClassCodeInfo = null;
  var next_sibling : ClassCodeInfo = null;

  var class_tag : Int = 0;
  var max_class_tag : Int = 0;

  def get_class_tag() : Int = class_tag;
  def get_max_tag() : Int = max_class_tag;
  def get_sibling() : ClassCodeInfo = next_sibling;

  def add_child(ci: ClassCodeInfo) : Unit = {
    ci.set_sibling(first_child);
    first_child = ci
  };
  def set_sibling(ci : ClassCodeInfo) : Unit = next_sibling = ci;
    
  var next_method : Int = 0;
  var next_attr : Int = 0;

  var method_array : ArrayAny = null;
  var attr_array : ArrayAny = null;

  def set_tags(ct : Int, methods : ArrayAny, attrs : ArrayAny) : Int = {
    class_tag = ct;
    next_method = methods.length();
    next_attr = attrs.length();
    class_decl.get_features().accept(this); // PHASE 1
    method_array = methods.resize(next_method);
    attr_array = attrs.resize(next_attr);
    class_decl.get_features().accept(this); // PHASE 2
    if (!is_null(first_child))
      max_class_tag = first_child.set_tags(ct+1,method_array,attr_array)
    else max_class_tag = ct;
    if (!is_null(next_sibling)) 
      next_sibling.set_tags(max_class_tag+1,methods,attrs)
    else max_class_tag
  };

  override def visit_Features_one(node:Feature) : Any = node.accept(this);

  override def visit_method(m:Cmethod,o:Boolean,name:Symbol,fs:Formals,
                            rtype:Symbol,expr:Expression) : Any = {
    if (!is_null(method_array))
      method_array.set(table.get_offset_info(m),m)
    else if (o) 
      table.set_info(m,table.get_offset_info(m.get_overrides()))
    else {
      table.set_info(m,next_method);
      next_method = next_method+1
    }
  };

  override def visit_attr(a:Cattr,name:Symbol,of_type:Symbol) : Any = {
    if (!is_null(attr_array))
      attr_array.set(table.get_offset_info(a),a)
    else {
      table.set_info(a,next_attr);
      next_attr = next_attr + 1
    }
  };

  var int_sym : Symbol = symbol("Int");
  var unit_sym : Symbol = symbol("Unit");
  var boolean_sym : Symbol = symbol("Boolean");

  def emit_classname_table(e : Emitter, st : StringTable) : Unit = {
    e.word(e.s_STRINGLIT(st.get(class_decl.get_name())));
    if (!is_null(first_child)) first_child.emit_classname_table(e,st)
    else ();
    if (!is_null(next_sibling)) next_sibling.emit_classname_table(e,st)
    else ()
  };
    
  def emit(e : Emitter) : Unit = {
    e.data();
    var cn : String = symbol_name(class_decl.get_name());
    var i : Int = 0;
    var n : Int = 0;
    e.label_def(cn.concat(e.s_DISPTAB()));
    i = 0; n = method_array.length();
    while (i < n) {
      var m : Cmethod = method_array.get(i) match { case m:Cmethod => m };
      var o : Cclass_decl = m.get_owner();
      e.word(e.s_METHOD(o.get_name(),m.get_name()));
      i = i + 1
    };
    if (!class_decl.get_inheritablep()) {
      e.global(cn.concat(e.s_PROTOBJ()))
    } else {};
    e.obj_header(cn.concat(e.s_PROTOBJ()),cn,class_tag,next_attr);
    i = 0; n = attr_array.length();
    while (i < n) {
      var a : Cattr = attr_array.get(i) match { case a:Cattr => a };
      var t : Symbol = a.get_of_type();
      if (t == int_sym) {
	e.word(e.s_INTLIT(0))
      } else if (t == boolean_sym) {
	e.word(e.s_FALSE())
      } else if (t == unit_sym) {
        e.word(e.s_UNITLIT())
      } else {
	e.word(0)
      };
      i = i + 1
    };
    if (!is_null(first_child)) first_child.emit(e) else ();
    if (!is_null(next_sibling)) next_sibling.emit(e) else ()
  };
}


class SymbolIntTable() extends IO()
{
  var table : Hashtable = new Hashtable();
  var next : Int = 0;

  def add(sym : Symbol) : Unit =
    table.get(sym) match {
      case null => table.put(sym,next); next = next + 1
      case i:Int => ()
    };

  def get(sym : Symbol) : Int =
    table.get(sym) match {
      case i:Int => i
    };

  def emit_one(e : Emitter, k : Symbol, i : Int) : Any = abort("abstract!");

  def emit(e : Emitter) : Unit = {
    e.data();
    var enum : Enumeration = table.elements();
    while (enum.hasNext()) {
      var p : Pair = enum.next() match { case p:Pair => p };
      var k : Symbol = p.first() match { case s:Symbol => s };
      var i : Int = p.second() match { case i:Int => i };
      emit_one(e,k,i)
    }
  };
}

class IntTable(var tag : Int) extends SymbolIntTable()
{
  def add_int(i : Int) : Unit = add(symbol(i.toString()));

  def get_int(i : Int) : Int = get(symbol(i.toString()));

  override def emit_one(e : Emitter, k : Symbol, i : Int) : Unit = {
    e.obj_header(e.s_INTLIT(i),"Int",tag,1);
    e.word(symbol_name(k));
    ()
  };

  { add_int(0) }; // force 0 to have index 0
}

class StringTable(var tag : Int, var it : IntTable) extends SymbolIntTable()
{
  override def add(sym : Symbol) : Unit = {
    super.add(sym);
    it.add_int(symbol_name(sym).length())
  };

  def add_string(s : String) : Unit = add(symbol(s));

  var first_printable : Int = 32;
  var last_printable : Int = 126;
  var quote : Int = "\"".charAt(0);
  var backslash : Int = "\\".charAt(0);

  override def emit_one(e : Emitter, k : Symbol, l : Int) : Unit = {
    var s : String = symbol_name(k);
    var n : Int = s.length();
    var sz : Int = (n + e.i_WORDSIZE())/e.i_WORDSIZE();

    e.obj_header(e.s_STRINGLIT(l),"String",tag,sz+1);
    e.word(e.s_INTLIT(it.get_int(n)));

    var i : Int = 0;
    while (i < n) {
      var ch : Int = s.charAt(i);
      if (if (ch < first_printable) false
	  else if (last_printable < ch) false
	  else if (ch == quote) false
	  else if (ch == backslash) false
	  else true) {
	e.ascii();
	e.out(s.substring(i,i+1))
      } else {
	e.byte(ch)
      };
      i = i + 1
    };
    e.byte(0);
    e.align()
  };

  { add_string("") }; // force "" to be string_lit 0
}

class FillLitTables(var st : StringTable, var it : IntTable)
extends CoolTreeVisitor()
{
  override def visit_class_decl(the_node:Cclass_decl,name:Symbol,parent:Symbol,features:Features,filename:Symbol) : Any = {
    st.add(name);
    st.add(filename);
    super.visit_class_decl(the_node,name,parent,features,filename)
  };

  override def visit_int_lit(the_node:Cint_lit,token:Symbol) : Any = {
    it.add(token)
  };

  override def visit_string_lit(the_node:Cstring_lit,token:Symbol) : Any  = {
    st.add(token)
  };
}
// #)
// PA6 Helper classes for inheritance graph and literal tables

// ## PA7: helper class for computing temps
class TempCalculator(var table: CodeInfoTable) extends CoolVisitor() {
  var max: Int = 0;

  // Update max value
  def set_max(curr: Int): Unit =
    if (max < curr) max = curr else ();

  // Get max value
  def get_max(): Int = max;

  def calc(e: Expression, curr: Int): Unit = {
    // Takes in a CoolNode and sets it in the table with the current value
    table.set_info(e, curr);
    // Sets the max as the current
    set_max(curr);
    e match {
      case n: Cno_expr => 
      case v: Cvariable => 
      case a: Cassign => (calc(a.get_expr(), curr))
      case d: Cdispatch => (calc(d.get_expr(), curr))
      case d: Cstatic_dispatch => (calc(d.get_expr(), curr + 1))
      case c: Ccond => calc(c.get_pred(), curr); calc(c.get_then_exp(), curr); calc(c.get_else_exp(), curr)
      case l: Cloop => (calc(l.get_body(), curr))
      case c: Ctypecase => (calc(c.get_expr(), curr + 1))
      case b: Cblock => (calc(b.get_body().elements().next(), curr))
      case l: Clet => (calc(l.get_body(), curr + 1))
      case op: Cadd => calc(op.get_e1(), curr); calc(op.get_e2(), curr + 1)
      case op: Csub => calc(op.get_e1(), curr); calc(op.get_e2(), curr + 1)
      case op: Cmul => calc(op.get_e1(), curr); calc(op.get_e2(), curr + 1)
      case op: Cdiv => calc(op.get_e1(), curr); calc(op.get_e2(), curr + 1)
      case op: Cneg => (calc(op.get_e1(), curr))
      case op: Clt => calc(op.get_e1(), curr); calc(op.get_e2(), curr)
      case op: Cleq => calc(op.get_e1(), curr); calc(op.get_e2(), curr)
      case op: Ccomp => (calc(op.get_e1(), curr))
      case l: Cint_lit =>
      case l: Cbool_lit =>
      case l: Cstring_lit =>
      case a: Calloc =>
      case u: Cunit => 
      case n: Cnil => 
    }
  };

  def calc_binary(e1: Expression, e2: Expression, curr: Int): Unit = {
    // TODO: continue with expressions with appropriate offsets
    var old_offset = current_offset
    var restore = old_offset
    calc(e1, current_offset)
    calc(e2, current_offset + 1)
    restore = current_offset
    ()
  };

  var current_offset: Int = 0;

  def calc_exprs(es: Expressions, curr: Int): Unit = {
    set_max(curr);
    // TODO: continue with expressions but backup/restore current_offset before/after visit sequence
    var old_offset = current_offset
    var restore = old_offset
    es.accept(this)
    restore = current_offset
    ()
  };

  def calc_cases(cs: Cases, curr: Int): Unit = {
    set_max(curr + 1); // in case there are no cases
    // TODO: continue with cases but backup/restore current_offset before/after visit sequence
    var old_offset = current_offset
    var restore = old_offset
    cs.accept(this)
    restore = current_offset
    ()
  };

  override def visit_Expressions_one(e: Expression): Unit = calc(e, current_offset);

  override def visit_Cases_one(c: Case): Unit =
    c match {
      case b: Cbranch => {
        table.set_info(b, current_offset);
        // TODO: continue with branch expression with appropriate offset
        calc(b.get_expr(), current_offset + 1)
      }
    };
}
