
def _new_generator_command(ctx, out_file, jars):
  java_path = ctx.attr._jdk[java_common.JavaRuntimeInfo].java_executable_exec_path
  gen_cmd = str(java_path)
  
  generator_class = ctx.attr.generator_class

  sep = ";" if ctx.attr.is_windows else ":"

  gen_cmd += " -cp \"{jars}\" {generator_class} --schema {schema} --out {out_file}".format(
    java = java_path,
    jars = sep.join(jars),
    generator_class = generator_class,
    schema = ctx.file.schema.path,
    out_file = out_file.path
  )
  # print(ctx.file.schema.path)
  # print(out_file.path)
  return gen_cmd
  # return "cat {one} > {two} && cat {three} > /tmp/ooo".format(
  #   one = ctx.file.schema.path, 
  #   two = out_file.path,
  #   three = ctx.file.schema.path,
  # )

def _impl(ctx):
  targets = ctx.attr.deps
  jars = [o for t in targets for o in t.outputs if o.endswith(".jar")]
  out_file = ctx.actions.declare_file("%s.scala" % (ctx.attr.name))

  ctx.actions.run_shell(
    inputs = [ctx.file.schema],
    command = _new_generator_command(ctx, out_file, jars),
    outputs = [out_file],
    tools = ctx.files._jdk
  )

  srcs = out_file.path

  return DefaultInfo(files = depset([out_file]))

_spice4s_generator = rule(
  attrs = {
    "deps": attr.label_list(),
    "generator_class": attr.string(default = "spice4s.generator.GeneratorCli"),
    "is_windows": attr.bool(mandatory = True),
    "schema": attr.label(mandatory = True, allow_single_file = True),
    "_jdk": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_runtime"), cfg = "host"),
  },
  implementation = _impl
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
