# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml) [![Elm package](https://img.shields.io/elm-package/v/arowM/tepa)](https://package.elm-lang.org/packages/arowM/tepa/latest/)

[にほんご🥰(Japanese version)](https://github.com/arowM/tepa/blob/main/README-ja.md)

( Please give me a star on [GitHub](https://github.com/arowM/tepa/tree/main)! )  
![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

TEPA (pronounced /tíːpə/) is a framework for building robust, user-centric Web applications.

## Too boring for tech geeks.

TEPA is **extremely boring for tech geeks**. If you are interested in TEPA for any of the following reasons, please leave **before you waste your time**.

- Because TEPA uses my favorite programming language
  Unfortunately, we **did not** adopt the programming language because we liked it. We did not want to use such a language, but we had no choice but to adopt it because the advantages were slightly greater than adopting other languages under the current circumstances. We **did not** create TEPA for the language. Please leave before you feel bad.
- Because I am interested in the XX paradigm / Because I want to learn about the XX paradigm
  Unfortunately, you will not get any interesting information or experience about the XX paradigm. TEPA's only goal is "to create bug-free web applications that provide good user experiences", so it uses an appropriate, but boring, and non-novel method to achieve the goal.
- Because I am interested in the difference between the XX Architecture and TEPA
  Is the comparison really made up for with the exact same purpose as TEPA? If you are building data-centric applications that can be built with the XX Architecture, then you are wasting your time learning TEPA. If you are not most interested in thinking about the user experience, there is no need to learn TEPA.

**Please understand the meaning why this is at the beginning of the README. **

If you want to express your "cool" opinion on social networking sites or blogs, such as "I learned TEPA and didn't like it" or "TEPA is technically different from XX in this point🤓", it is just the same as saying "I am an incompetent who is not capable of reading warnings written at the very beginning of README, but I criticize TEPA without realizing I am an idiot💩".
TEPA is not developed with the motives of someone's likes or dislikes, technical interests, and so on. It simply adopts the method that has the greatest benefit after weighting its advantages and disadvantages against its goals and constraints. If your interests lie elsewhere than in TEPA's goals, you are not in the right place to talk about TEPA.

If TEPA's goals were truly aligned with your goals, you would have something to do before baring your filthy self-consciousness
on social networking sites; you can politely create an issue on GitHub as follows.

> For TEPA's purposes, the existing method has these possible advantages and disadvantages. In contrast, my proposed method seems to have greater advantages, but on what grounds does TEPA adopt this method?

## Who is the Web Application for?

Are components the main actors in a Web application? Or objects? Functions? Data?
Web applications are for the _users_ who use them. Sure, the owner is the service provider, but if you want the users to behave the way you want, you have to pay attention to how users think and feel.

**If you don't think so, leave this page now. You should love your favorite technology. **

What about modern web application development? In particular, the front-end, which is supposed to be directly related to the user, tends to focus on things that have nothing to do with the user's mind, such as components and data states, though.

Even the DOM API of Web browsers is dominated by DOM elements:

```js
window.addEventListener("load", () => {
  document.getElementById("submitButton").addEventListener("click", (event) => {
    // some code
  });
});
```

In the above example, the main subject is the DOM element for the submit button. In such a design, it is not easy to get the information that is a natural requirement in an application: "Under what circumstances and after what events in the past did the user press this button?" To get such information, we have to prepare a number of flags and variables, update them, analyze them all carefully, and then say like a detective, "Judging from the situation, this could possibly be a homicide."

## Stateless, but Contextful

I am sure that 95% of people would recognize the term "UX" as a cool way of saying UI, but in fact it means "user experience". Since it is an "experience," it is based on the accumulated experiences of users.
Ideally, to think about UX, you should go back to the kind of family he or she grew up in, the kind of community he or she was exposed to, and the kind of education he or she received in school. However, in today's application development, it is not easy to analyze even the context in which the user operates the application after it is loaded.

The concept of _stateless_, which is so valued in application development these days, is an idea to reduce bugs by successfully isolating various states, such as the current time, that surround the application. On the other hand, if we isolate the context as well as states, it is hard to develop user-centric applications.

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

TEPA uses an exception-free language. With the language, you are forced by the compiler to handle all unexpected things on the fly. It may seem rigid and depressing at first. But one day you will be grateful for this nagging compiler.
The language is a kind of DSL (domain specific language) for the web front end. The language specification is small, so the you can learn quickly, and risky features such as exceptions have been stripped away, so a certain level of quality can be guaranteed no matter who writes it.

Want to use TypeScript or JavaScript? That would be another good choice. We will not interfere in any way with your choice, so you should not interfere in any way with ours.

## Automatically test your scenarios.

If you are developing an application with the same goal as TEPA, you should always consider _use case scenarios_. A _use case scenario_ is a written description of how a user thinks, how he/she operates, and how the system responds to those operations. TEPA is not necessary for those who write programs without thinking about it, or for those who are not interested in such things and only want to concentrate on the coding process. **Please go home. **

TEPA provides a feature for creating use case scenarios to achieve such a goal. With this scenario creation feature, you can create a scenario as a program. The scenario can be displayed in a browser in a nice format, or output as Markdown. Of course, you can write scenarios in the way you are used to. Even if you do not use this feature, TEPA will help you to realize your use case scenario as a program.

Even more amazing is that the scenario can be turned directly into a lightweight test: write an application and a scenario in TEPA, and you can test whether it behaves as described in the scenario by emulating the behavior of the application. This testing is instantaneous because it is not done using a real web browser. For example, if you have a process that sleeps for 500 million years, you don't have to go into a cold sleep betting that the Earth will still be around in 500 million years. There is also no need to prepare mocks. Even for processes that depend on external conditions at runtime, such as requests to back-end servers or random number generation within the application, you can write the expected behavior in a scenario, and the automatically generated test will pass the results to the application for further testing.
Of course, being an emulator has its drawbacks. It does not actually render the screen, so visual regression testing is not possible. For the important parts, you should use something like Playwright or something similar.

## Being Politically Incorrect

You may have felt "Hmmm, this guy is a fool 🤓" or "Are you trying to pick a fight? 😡" while reading this document. If you felt this way, then you are not a target user for TEPA. How can this framework document, which repeatedly stresses the importance of _considering the feelings of the user_, not consider the feelings of the readers?
If you make judgments based only on what you can see now, without trying to guess the background and reasons for such a peculiar technical document, then TEPA's goal of "user-centered application development" is not for you.
Why are you still reading this? I warned you in the beginning. Go away!

There are some industries in the world that force all works and organizations to follow a rule called "diversity" in order to be politically correct. In such industries, there is an overall loss of diversity, as all works and organizations become the same form according to the "diversity" as they think.
Even in languages and frameworks, there are many technologies that are community-driven and democratically managed with different opinions from different users, resulting in a loss of opinion and a loss of what they are.

TEPA, on the other hand, does not accept diverse opinions. Has humanity ever said, "Let's create a society that is easy to live in for creatures with two fingers and creatures with four stomachs"? NOOOOOO! 💢🐐

TEPA does not have to be a fad. There is noo problem if only a few creatures find value in it. That is how we will contribute to the diversity of the world.

## How to get started

Humans seem to love tutorials. You know, the ones where you follow the instructions and you just feel like you understand everything. Unfortunately, TEPA does not have such a tutorial yet.

However, there is already a [Getting Started Guide](https://package.elm-lang.org/packages/arowM/tepa/latest/Tepa), which also serves as API documentation, and a [Sample App](https://github.com/arowM/tepa-sample) are already available. If you are a programmer in the habit of reading and understanding the official documentation, you will have no trouble.

If you are a programmer who can read and understand what is written exactly as it is written, without being bound by stereotypes such as "If it were XX, I would do it this way", you will have no particular difficulty learning.
