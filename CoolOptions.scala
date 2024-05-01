class CoolOptions()
{
  // Boolean flags

  var scan_debug : Boolean = false;
  def get_scan_debug() : Boolean = scan_debug;
  def set_scan_debug(f : Boolean) : Unit = scan_debug = f;

  var parse_debug : Boolean = false;
  def get_parse_debug() : Boolean = parse_debug;
  def set_parse_debug(f : Boolean) : Unit = parse_debug = f;

  var semant_debug : Boolean = false;
  def get_semant_debug() : Boolean = semant_debug;
  def set_semant_debug(f : Boolean) : Unit = semant_debug = f;

  var analysis_debug : Boolean = false;
  def get_analysis_debug() : Boolean = analysis_debug;
  def set_analysis_debug(f : Boolean) : Unit = analysis_debug = f;

  var cgen_debug : Boolean = false;
  def get_cgen_debug() : Boolean = cgen_debug;
  def set_cgen_debug(f : Boolean) : Unit = cgen_debug = f;

  var scan : Boolean = false;
  def get_scan() : Boolean = scan;
  def set_scan(f : Boolean) : Unit = scan = f;

  var parse : Boolean = true;
  def get_parse() : Boolean = parse;
  def set_parse(f : Boolean) : Unit = parse = f;

  var semant : Boolean = true;
  def get_semant() : Boolean = semant;
  def set_semant(f : Boolean) : Unit = semant = f;

  var analyze : Boolean = false;
  def get_analyze() : Boolean = analyze;
  def set_analyze(f : Boolean) : Unit = analyze = f;

  var optimize : Boolean = false;
  def get_optimize() : Boolean = optimize;
  def set_optimize(f : Boolean) : Unit = optimize = f;

  var codegen : Boolean = true;
  def get_codegen() : Boolean = codegen;
  def set_codegen(f : Boolean) : Unit = codegen = f;

  var enable_gc : Boolean = false;
  def get_enable_gc() : Boolean = enable_gc;
  def set_enable_gc(f : Boolean) : Unit = enable_gc = f;

  var right_to_left_children : Boolean = false;
  def get_right_to_left_children() : Boolean = right_to_left_children;
  def set_right_to_left_children(f : Boolean) : Unit = right_to_left_children = f;

  var gen_c : Boolean = false;
  def get_gen_c() : Boolean = gen_c;
  def set_gen_c(f : Boolean) : Unit = gen_c = f;


  // String flags

  var basic_file:String = "/usr/local/lib/basic.cool";
  def get_basic_file() : String = basic_file;
  def set_basic_file(f : String) : Unit = basic_file = f;

  var analysis : String = "resolve:inline:dead-let";
  def get_analysis() : String = analysis;
  def set_analysis(f : String) : Unit = analysis = f;

  var out_filename : String = null;
  def get_out_filename() : String = out_filename;
  def set_out_filename(f : String) : Unit = out_filename = f;


  // Int flags

  var max_errors : Int = 50;
  def get_max_errors() : Int = max_errors;
  def set_max_errors(f : Int) : Unit = max_errors = f;

  var strip_attributes : Int = 0;
  def get_strip_attributes() : Int = strip_attributes;
  def set_strip_attributes(f : Int) : Unit = strip_attributes = f;
}
