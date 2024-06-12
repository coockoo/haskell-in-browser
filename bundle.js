const main = async () => {
  const mod = WebAssembly.compileStreaming(fetch('todo.wasm'));
  const instance = await WebAssembly.instantiate(mod);
  console.log(instance.exports.addTodo());
};

main();
