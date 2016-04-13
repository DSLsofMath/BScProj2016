Interview with Chris Sells
==========================

Link: <https://channel9.msdn.com/blogs/charles/expert-to-expert-martin-fowler-and-chris-sells-perspectives-on-domain-specific-languages>

A programming language with limited expressiveness
--------------------------------------------------

Value
-----

The main goal of DSLs (according to Fowler) is to enable a deeper communication between a programmer and a domain expert (such as a graphical interface designer) and therefore make their collaboration more productive.
If the main goal is not reached, a DSL could still be considered a success if it enabled the programmer to be more productive due to a more specific structure, even if the DSL failed to appeal to the domain expert.

Language Workbench
------------------

A bundle of tools, languages and sample execution that is easy to build and distribute.

Martin Fowler's talk on DevCon 2009
===================================

Link: <http://download.microsoft.com/download/E/7/7/E77A8FCE-0362-4930-BD5E-8A21EC77E38D/01WelcomeAndKeynote.wmv>

Difference between External (standalone) DSLs and Internal (embedded) DSLs
--------------------------------------------------------------------------

"With an external DSL you use any syntax you like and you treat it with a parser. And with an internal DSL \[it\] is really a idiomatic usage of one programming language. You are still writing in that host programming language, but you use only a subset of its features and you use it in a pidgin way to get that same feel that you could get with an external DSL."

-   Martin Fowler (Paraphrased)

He then continues and says that what the programmer should experience when using the DSL is that the DSL (it doesn't matter whether it is internal or external) is written specificly for the domain that the programmer is using it for.

Difference between Internal DSLs and "ordinary" APIs
----------------------------------------------------

If we consider internal DSLs as an idiomatic usage of a host language and that when "parsed" is nothing else than method/function calls in the host language and not something that parsed and then translated into another language, then what is the difference between an internal DSL and an API?

According to Fowler there is no clear line between what is a DSL and what is an API, however we may call an API a DSL if its syntax is built in such a way that it is possible to phrase semantic sentences. That is if the syntax of the API is easy to read and understand without the need to puzzle the code too much, we could call that library / API a DSL.

Difference between External DSLs and General Purpose Programming Language
-------------------------------------------------------------------------

If we consider external DSLs as something that is parsed and then may be executed in a way or may not be, then what is the difference between external DSLs and general purpose programming languages?

Take R for example (a programming language focused on the statistics domain), it is a general purpose programming language. You can do the same things with it as with many other languages, but it is focused on a particular domain, namely statistics, is it therefore a DSL? Another example is Java, which in that case could be said to be a DSL for programs running on the JVM.

Clearly, this distinction does not work because it is not precise enough. What Fowlers definition proposes is rather that a DSL has its expressiveness limited and focused on a particular domain.

With that definition a DSL will not allow you to do anything you could expect of a programming language and it will probably not be *Turing complete*, in fact it should probably not be *Turing complete* because that would enable programmers to use it for purposeses it was not intended for.

Fowler's Definition:
--------------------

With the discussion above the following definition should make sense and feel like a sensible definition.

**Domain Specific Language** (noun)

-   a computer programming language of limited expressiveness focused on a particular domain.
