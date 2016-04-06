What is this text?
==================

This is some notes taken when reading *Communicating Mathematics (1995)* by Charles Wells. If not stated otherwise everything in this text is based on the article and all ideas are credited to Charles Wells.

Notes
=====

Sets as the fundamental building block in mathematics
-----------------------------------------------------

We are often taught that sets are the most fundamental object in mathematics and that all other objects can be viewed as nothing else as a complicated set.

It is important to remember that this is only representation and that not how mathematics are discussed in practice.

For instance in *Communicating Mathematics*, Charles Wells argues that a mathematician would be confused if we would ask him which points in the plane had nonempty intersections with the point (3,2). Since we are talking about operations on sets and applying them to points it is a meaningless question even though a point could be represented as a set.

Use specifications rather than definitions when describing mathematical objects
-------------------------------------------------------------------------------

It is probably more interesting to present the relations a object has to other objects and how they interact together rather than trying to give the reader/student a fair image of what the object "really is". This is because the formal definition of an object or concept might require us to provide unnecessary details that are used for other purposes and that obscure the how the concept is used in practice and how it relates to everything else.

Syntax and semantics
--------------------

In *Communicating Mathematics* Charles Wells states that:

``` 
"/Syntax/ is the study of expressions in linguistics or
computer science, and /semantics/ is the study of how 
meaning is assigned to expressions."
```

A platonistic view argues that a mathematical object is represented by a mathematical expression. Thus, the expression itself is not the same as the object and more than one expression may denote the same object.

Another view in mathematics argue that expressions are themselves and that we may manipulate these expressions according to a set of precise rules. Those who hold to this view would presumably agree that there are some expressions that are equivalent (that is, denoting the same object from a platonistic view).

According to Charles Wells, a common problem is that students and teachers in mathematics often hold to different views regarding mathematical expressions, teachers mostly agree to the platonistic view and student more often only see the notation and expressions and not the objects and relation they denote.

There are two ways to go about this problem in communication, either we start being more explicit about the structure and meaning of mathematical expressions *or* we stop talking like platonists and teach mathematics as a game of manipulating expressions. With the latter choice, mathematics will be only syntax, with clear rules of how one can manipulate the syntax.

Formal transformations
----------------------

Also called *rewrite rules*, states how we can manipulate an expression.

Example:

```
a + bx + by <=> a + b(x + y)
```

Sometimes formal transformations are only allowed in specific applications, i.e they are "context sensitive", but they can also be free of such constraints and are in that case "context free".

Formal transformations are comparable to macros in computer languages, where we specify statements that should be replaced with something else by a preprocessor.

Often students can get a long way when trying to prove something by replacing the text in problem with the text of the given definitions.

In an example of proving that in an ordering of real numbers it is true that if (x &gt; z) then (x &gt; y) ∨ (y &gt; z), Wells showed that proofs can be written in a semantical or a syntactic way.

If the proof is written using syntax rather than semantics it is easy to make sure that every step is allowed and correct since it only formal transformations that are used and we have specific rules governing what transformations are allowed, however it does not convey much of a mental representation of what the problem or proof is about, it is more an expression that should be proven not to contradict itself.

If the proof is written in a semantic way rather than syntactic it could be harder to verify that a proof is correct, especially if it written by someone whose mental representation is drastically different from our own. A semantic proof does however convey a mental representation and could therefore be more helpful in understanding a problem or concept.

All mathematicians do not agree which of the way should be used in writing proofs, some argues that mathematics is all about syntax and therefore the mental representation is unnecessary and other mathematicians argue that the best way to teach students mathematics is to show them the right mental representation so they get intuition and understanding for the objects.

Types and polymorphism
----------------------

In the article it is noted that while programming languages have clear types and that a compiler usually complains when the types do not align, in mathematics this is not the case.

In mathematics the same operator may be defined on many different types and mean completely different things. An example from the article is "×" which is defined to represent multiplication of numbers if we are talking about scalar values, but is the cartesian product in the world of sets and also represents the vector product in three-dimensional vectors.

Often the types in mathematics are not explicit and have to be inferred from the context, which could be tedious if the students are not used to it.

Self-monitoring
---------------

In the computer science / hacker world there are a lot of slang and jargon describing the mental states or behaviour while working. This is clever because a reader may recognise the feeling or description and therefore remember it better.

Wells argue that mathematicians should come up with names for states and behaviours, but also for common mistakes made in mathematics, again by putting a label on them it might be easier to remember them and therefore avoid them.

It is also noted in the text that mathematicians for some reason tended (the article is old now, I have not double-checked this) to have low-context conversations, that is they do not assume that anything is already known by the other part and makes everything explicits to avoid miscommunication. Why this is the case though is unclear, there are potential for saving time for both parts if some parts are assumed to be common knowledge.

Recommendations from Communicating Mathematics
==============================================

The article gives a bunch of recommendations and tips to teachers and lecturers on how to communicate mathematics. I have listed them in the order they appear in the text for simplicity.

-   Explain a basic concept by giving a specification of the concept.
-   Teachers should make the distinction between syntax and semantics explicit.
-   Introduce informal parsing of mathematical expressions as a learning tool.
-   Make explicit the allowable syntax for statements about a type of object.
-   Encourage students to begin proving a theorem by replacing (some or all of) the words that have definitions with the text of their definitions.
-   Transmit your mental representation of concepts whenever you can, but *also* give proofs as explicit logical calculations when appropriate, because that provides the student with a second way to deal with the problem and provides him or her with the tools to carry out similar proofs.
-   Give explicit rules of inference for concepts when they are introduced.
-   Use the concepts of type and polymorphism explicitly to help sutdents to understand and avoid the traps of type confusion.
-   Expect conceptual understanding at the appropriate level from all students in any course, and test them on it.
-   Describe and *name* the common kinds of mistakes students make.
