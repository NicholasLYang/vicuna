function print(...args) {
  for (const arg of args) {
    if (Array.isArray(arg)) {
      console.log("[");
      for (const elem of arg) {
        print(elem);
      }
      console.log("]");
    } else if (arg.hasOwnProperty("__type__")) {
      if (arg.hasOwnProperty("__enum__")) {
        console.log(`${arg.__enum__}::${arg.__type__}`);
      } else {
        console.log(arg.__type__);
      }
    } else {
      console.log(arg);
    }
  }
}
