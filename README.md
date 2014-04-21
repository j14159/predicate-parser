Predicate Parser
================
Some poorly directed experiments in search of a few different things.

I've been rambling occassionally about liking Erlang's pattern matching and Scala's case classes for a while now.  This started as something of an attempt to chase part of what [Paul Chiusano](https://twitter.com/pchiusano) talked about [a while ago](http://pchiusano.blogspot.ca/2011/12/future-of-programming.html) and is spiralling out into a few directions.  It was originally of the form "it might be cool if function prototypes were Prolog-ish facts" and is now listing in the direction of "a knowledge base/relational store instead of a pile of source files" but is nowhere close to that yet.

I'd like to build an interpreter for the AST that's slowly growing (inspired somewhat by [AIM-349](http://dspace.mit.edu/bitstream/handle/1721.1/5794/AIM-349.pdf) and [definitional interpreters](http://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other)) but I'm beginning to suspect that this will only ever be a parser in which I make some mistakes that inform some other attempt in future.  Consider this an unstable playground.

Things I'm thinking of attempting with this but may do in a cleaner/better thought out future project:

* CSP in the vein of golang channels in Erlang processes
* tail call optimization (environments from the two papers mentioned above being essentially stacks)
* static typing with inference given a set of concrete BIFs with known return types
