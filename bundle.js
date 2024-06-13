import { WASI } from 'https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/+esm';

const main = async () => {
  const wasi = new WASI([], [], []);

  // const t = await fetch('Counter.wasm');
  // const buf = await t.arrayBuffer();
  const mod = await WebAssembly.compileStreaming(fetch('Counter.wasm'));
  console.log('mod', mod);
  const instance = await WebAssembly.instantiate(mod, {
    wasi_snapshot_preview1: wasi.wasiImport,
    imports: { fib: (a) => console.log('fib', a) },
  });
  wasi.inst = instance;
  instance.exports.hs_init(0, 0);
  console.log(instance.exports.fib(10));
};

main();
