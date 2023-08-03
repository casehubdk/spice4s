
def _new_generator_command(ctx, out_dir, jars):
  java_path = ctx.attr._jdk[java_common.JavaRuntimeInfo].java_executable_exec_path
  gen_cmd = str(java_path)
  
  generator_class = ctx.attr.generator_class

  sep = ";" if ctx.attr.is_windows else ":"

  gen_cmp += " -cp \"{jars}\" {generator_class} --schema {schema} --out {out_file}".format(
    java = java_path,
    jars = sep.join(jars),
    generator_class = generator_class,
    schema = ctx.file.schema.path,
    out_dir = out_dir.path
  )
  return gen_cmp

def _impl(ctx):
  targets = ctx.attr.deps
  jars = [o for t in targets for o in t.outputs if o.endswith(".jar")]
  out_dir = ctx.actions.declare_file("%s" % (ctx.attr.name))

  ctx.actions.run_shell(
    inputs = ctx.file.schema,
    command = _new_generator_command(ctx, out_dir, jars),
    outputs = [out_dir],
    tool = ctx.files._jdk
  )

  srcs = out_dir.path

  return DefaultInfo(files = depset([out_dir]))

_spice4s_generator = rule(
  attrs = {
    "deps": attr.label_list(),
    "generator_class": attr.string(mandatory = True, default = "spice4s.generator.cli.GeneratorCli"),
    "is_windows": attr.bool(mandatory = True),
  }
)

def spice4s_generator(name, **kwargs):
  _spice4s_generator(
    name = name,
    is_windows = select({
        "@bazel_tools//src/conditions:windows": True,
        "//conditions:default": False,
    }),
    **kwargs
  )
