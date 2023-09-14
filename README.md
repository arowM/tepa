# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml)  
[Document](https://package.elm-lang.org/packages/arowM/tepa/latest/)  
[にほんご🥰(Japanese version)](./README-ja.md)

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

TEPA (pronounced /tíːpə/) is a framework for building robust, user-centric Web applications.

## Who is the Web Application for?

Are components the main actors in a Web application? Or objects? Functions? Data?
Web applications are for the _users_ who use them. Sure, the owner is the service provider, but if you want the users to behave the way you want, you have to pay attention to how users think and feel.

What about modern web application development? In particular, the front-end, which is supposed to be directly related to the user, tends to focus on things that have nothing to do with the user's mind, such as components and data states, though.

Even the DOM API of Web browsers is dominated by DOM elements:

```js
window.addEventListener("load", () => {
  document.getElementById("submitButton").addEventListener("click", (event) => {
    // some code
  });
});
```

In the above example, the main subject is the DOM element for the submit button. In such a design, it is not easy to get the information that is a natural requirement in an application: "Under what circumstances and after what events in the past did the user press this button? To get such information, we have to prepare a number of flags and variables, update them, analyze them all carefully, and then say like a detective, "Judging from the situation, this could possibly be a homicide."

## No exceptions

Exceptions are inventions that impede the development and prosperity of humanity. We painfully understand that you certainly do not want to think about the unexpected. You must want to put it off. But it is you who will get into trouble later.
Suppose you are building an airplane. Wouldn't you carefully inspect every single screw and replace it right away if it was defective? If the airplane fell down after it was all assembled, you would never know what actually caused the problem because most of the context would be lost. Exception is convenient, but it is such a risky technique.

Another bad part of exceptions is that they mess with static types. Even though TypeScript introduces static types to JavaScript, you still have to deal with values of `unknown` types that you have no way of knowing what they are at compile time.

TEPA uses an exception-free language called Elm. With Elm, you are forced by the compiler to handle all unexpected things on the fly. It may seem rigid and depressing at first. But one day you will be grateful for this nagging compiler.

## Automatically test your scenarios.

As mentioned earlier, in Web application development, it is essential to consider the mind of the user. TEPA provides a function for creating use case scenarios, which is a written description of how a user thinks and operates, and how the system responds to that thinking and operation.

Even more amazing is that the scenario can be turned directly into a lightweight test: write an application and a scenario in TEPA, and you can test whether it behaves as described in the scenario by emulating the behavior of the application. This testing is instantaneous because it is not done using a real web browser. For example, if you have a process that sleeps for 500 million years, you don't have to go into a cold sleep betting that the Earth will still be around in 500 million years. There is also no need to prepare mocks. Even for processes that depend on external conditions at runtime, such as requests to back-end servers or random number generation within the application, you can write the expected behavior in a scenario, and the automatically generated test will pass the results to the application for further testing.
Of course, being an emulator has its drawbacks. It does not actually render the screen, so visual regression testing is not possible. For the important parts, you should use something like Playwright or something similar.

## How to get started

Although tutorials for TEPA are currently under construction, a sample application is already available. Please see the contents of the [spa-sample directory](https://github.com/arowM/tepa/tree/main/spa-sample).

The [Elm language specification](https://guide.elm-lang.jp/core_language.html) is very small and easy to learn, and with the help of type annotations, reading TEPA's [module documentation](https://package.elm-lang.org/packages/arowM/tepa/latest/Tepa) makes it easy to understand `spa-sample` codes.

From the horizontal eyes of Sakura-chan, the goat, it seems that humans are creatures desperate to gain an advantage over others. Don't wait for the tutorial to be prepared, master TEPA before other humans do!
