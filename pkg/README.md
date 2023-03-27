# vicuna

Vicuna is a language that lets you write elegant, type-safe code that compiles to JavaScript. It should feel like
a tasteful blend of JavaScript, Rust, and OCaml.

## Status

This is a total work in progress. Anything written on here should be interpreted as a future aspiration and not as
the current reality for Vicuna.

The name isn't even set in stone. I just liked the sound and wanted a name that referenced camels.

## Why

Why am I writing Vicuna? I've enjoyed using languages that compile to JavaScript like TypeScript and ReScript
(formerly ReasonML). Both offer such a compelling set of features like excellent JavaScript compatibility (TS)
and type inference (RS). However, I've felt that each language needed a little of the other language.
If ReScript *only* had good async support and some more editor compatibility, or if TypeScript *only*
had sum types and integer types, then I'd be satisfied.

I also like Rust a lot and would love to see a language that manages to keep that pragmatic use
of immutability and imperative programming even while not compromising on safety. Also, traits 
are excellent and make the developer UX nicer.

Finally, another goal is to make 
[JavaScript: The Good Parts](https://www.oreilly.com/library/view/javascript-the-good/9780596517748/), the language.
Not in the literal sense, but more in the vein of imagining what a TC39 unencumbered by backwards compatibility
could accomplish. What if we could get [do expressions](https://github.com/tc39/proposal-do-expressions) 
and runtime types? What if we could change JavaScript's implicit casts? Of course, I do not speak for TC39
or any delegate to TC39.

## Why Compile to JavaScript?
I believe that compiling to JavaScript is a severely underrated strategy. A lot of people are 
looking to WebAssembly as the standard compilation target for the web. I don't disagree for
certain use-cases like intense, native-like computation. I've written my own attempt at compiling
to WebAssembly, [Saber](https://github.com/nicholaslyang/saber). However, WebAssembly is still a
very...constrained compilation target. With Saber I wrote and rewrote multiple compilation passes,
with lots of static analysis and fancy symbol tables; I added my own runtime with garbage collection; 
I spend a lot of time thinking about memory layout. 

You see, the difficulty in compiling Saber is that Saber, the source language, is very different from WebAssembly, 
the target language. Saber has all these high level concepts like closures, garbage collection, strings,
while WebAssembly has low level concepts like linear memory, indices, and load/stores. Getting a compiler to translate
from one to the other is rather challenging. But you know what does have high level concepts like closures,
garbage collection and strings? JavaScript! By compiling to JavaScript I'm saving myself so much pain.
And I didn't even get to the nasty stuff around interop, calling Web APIs, or debugging.

Granted, I was and am still very new to compilers. I'm sure an experienced compiler engineer 
could knock out something like Saber with ease. Heck I have a few friends who could do it. 
But for my skills, compiling to JavaScript is the best plan.

More grandly, I'd agree with the aphorism to "always bet on JavaScript". JavaScript may be maligned, but
it's ubiquitous, and it works pretty darn well. It's a rare high level, dynamic language that performs
decently. It has some really great libraries (and some...less great ones). As optimistic as I am about WebAssembly,
I don't anticipate that we'll move away from JavaScript anytime soon.
