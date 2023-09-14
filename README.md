# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml)  
[Document](https://package.elm-lang.org/packages/arowM/tepa/latest/)  
[にほんご🥰(Japanese version)](https://github.com/arowM/tepa/blob/main/README-ja.md)

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

## Stateless, but Contextful

The word "UX" is a word that I am sure more than 95% of people today are only aware of as "a cool word for UI," but in fact it means "user experience. Since it is an "experience," it is naturally based on the accumulated experiences of users.
Ideally, the user's experience should be traced back to the kind of family he or she grew up in, the kind of community he or she was exposed to, and the kind of education he or she received at school. Yet, in today's application development, it is not easy to analyze even the context in which the user is operating the application after it has been loaded.

The concept of _stateless_, which is so valued in application development these days, is a valuable one. The idea is to reduce bugs by successfully isolating various states, such as the current time that surrounds the application. On the other hand, if we isolate the context as well, we cannot develop applications that are user-friendly.

TEPA is stateless, but contextful. Applications can be developed by following the flow of the user's actions.

```elm
tutorial =
    Tepa.sequence
        [ showWelcomeMessage
        , awaitUserReactionToWelcomMessage <| \response ->
            case response of
                PoliteUserReaction ->
                    priseUser
                EvilUserReaction ->
                    punishUser
        ]
```

## No exceptions

The exception is a useful feature, but it drops the context. We certainly don't want to think about the unexpected things. We want to put it off. I understand that feeling. But you are the one who will be in trouble later.
Suppose you are building an airplane. Wouldn't you carefully inspect every single screw and replace it right away if it was defective? If the airplane fell down after it was all assembled, you would never know what actually caused the problem because most of the context would be lost. Exception is convenient, but it is such a risky technique.

Another bad part of exceptions is that they mess with static types. Even though TypeScript introduces static types to JavaScript, you still have to deal with values of `unknown` types that you have no way of knowing what they are at compile time.

TEPA uses an exception-free language called Elm. With Elm, you are forced by the compiler to handle all unexpected things on the fly. It may seem rigid and depressing at first. But one day you will be grateful for this nagging compiler.

## Automatically test your scenarios.

As mentioned earlier, in Web application development, it is essential to consider the mind of the user. TEPA provides a function for creating use case scenarios, which is a written description of how a user thinks and operates, and how the system responds to that thinking and operation.

Even more amazing is that the scenario can be turned directly into a lightweight test: write an application and a scenario in TEPA, and you can test whether it behaves as described in the scenario by emulating the behavior of the application. This testing is instantaneous because it is not done using a real web browser. For example, if you have a process that sleeps for 500 million years, you don't have to go into a cold sleep betting that the Earth will still be around in 500 million years. There is also no need to prepare mocks. Even for processes that depend on external conditions at runtime, such as requests to back-end servers or random number generation within the application, you can write the expected behavior in a scenario, and the automatically generated test will pass the results to the application for further testing.
Of course, being an emulator has its drawbacks. It does not actually render the screen, so visual regression testing is not possible. For the important parts, you should use something like Playwright or something similar.

## How to get started

Humans seem to love tutorials. You know, the ones where you follow the instructions and you get something that looks good and you just feel like you understand everything. Unfortunately, TEPA does not have such a tutorial yet.

However, there is already a [Getting Started Guide](https://package.elm-lang.org/packages/arowM/tepa/latest/Tepa), which also serves as API documentation, and a [Sample App](https://github.com/arowM/tepa-sample) are already available. If you are a programmer in the habit of reading and understanding the official documentation, you will have no trouble.

From the horizontal eyes of Sakura-chan, the goat, it seems that humans are creatures desperate to gain an advantage over others. Don't wait for the tutorial to be prepared, master TEPA before other humans do!
