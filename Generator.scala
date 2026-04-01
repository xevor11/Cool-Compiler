class Generator() extends CoolVisitor() {
  def run() : Unit = {
    abort("abstract method")
  };
  def create(p : Program, options : CoolOptions, io : IO) : Generator =
    if (options.get_gen_c())
      new CGen(p,options,io)
    else if (options.get_optimize())
	   new OptGen(p,options,io)
    else
      new CodeGen(p,options,io);
}
