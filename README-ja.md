# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml) [![Elm package](https://img.shields.io/elm-package/v/arowM/tepa)](https://package.elm-lang.org/packages/arowM/tepa/latest/)

( [GitHub](https://github.com/arowM/tepa/tree/main)のスターちょうだい！ )  
![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

TEPA（ティーパ）

バグのない、よいユーザー体験を生むウェブアプリケーションのためのフレームワーク

## 技術的につまらない

TEPAは **技術的に途轍もなくつまらないもの** です。もしあなたが以下のような動機でTEPAに興味を持ったなら、このようなつまらない技術で「時間を無駄にした」と思う前に **お帰りください** 。

- 私の推し言語を使っているから
  残念ながらその言語はしかたなく採用しています。またその言語のために作ったフレームワークではなく、偶然都合がいいからその言語を選んだだけです。嫌な気持ちになる前にお帰りください。
- ○○型パラダイムに興味があるから / ○○型パラダイムについて学びたいから
  残念ながらあなたが想像する○○型パラダイムについて何もおもしろい情報も経験も得られません。TEPAはただ「バグのない、よいユーザー体験を生むウェブアプリケーション」を第一の目的とする方に、その目的を実現する適切でつまらない目新しさのない手法を提供するだけです。
- ○○アーキテクチャーとの違いに興味があるから
  その比較対象は果たしてTEPAと全く同じ目的をもって発明されたものなのでしょうか？ もしあなたが○○アーキテクチャーでも作れるデータ中心なアプリケーションを作っているなら、TEPAを学んでも無駄です。ユーザー体験を考えることに一番の興味がないなら、TEPAを学ぶ必要はありません。

**READMEの冒頭にこのことが書いてある意味を理解してください。**

もしあなたがSNSやブログで「TEPAを学んだが好きになれなかった」とか「TEPAは技術的に○○とはここが違う」などとピーチクパーチクするのであれば、それは「私はREADMEの一番最初に書いてある日本語を読む能力がない無能なのに、無知の知を自覚せず批判するおばかさんです💩」とプラカードを掲げていることと等しい行為です。
TEPAは好き嫌いだとか、宗派だとか、技術的なおもしろさとか、そんな動機では開発していません。目的と制約条件に対してメリットとデメリットを検討してメリットが大きい手法を採用しているだけです。もしあなたがTEPAの目的とは別のところに興味関心があるなら、あなたはTEPAについて語る場所にいません。

本当にTEPAの目的とあなたの目的が一致するなら、そもそもSNSだとかで騒ぐ前にやることがあるはずです。
「TEPAの目的からすると既存手法はこのようなメリットとデメリットが考えられる。対して私の提案手法はより大きなメリットがあるように思えるが、どのような経緯でTEPAはこの手法を採用していますか？」と、無知の知を体現したissueをGitHubに立てたらいいのです。

## ウェブアプリケーションは誰のためのもの？

ウェブアプリケーションの主役はコンポーネントでしょうか？ それとも、オブジェクト？ 関数？ データ？
いえ、ウェブアプリケーションはそれを使うユーザーのためのものです。もちろん所有者はサービスの運営者ですが、ユーザーの気持ちを考えることなしに自社の都合を押し付けても、ユーザーは思い通りに動いてくれないでしょう。言うまでもなく、開発においてもユーザーの気持ちを考えることが最も大切です。

**そう思えないなら、今すぐこのページを去ってください。あなたはあなたの好きな技術を愛してください。**

一方で現代のウェブアプリケーション開発はどうでしょうか？ 特にユーザーと直接かかわるはずのフロントエンドは、コンポーネントとか、データ状態とか、そういうユーザーの心とは関係ないものを中心に考えがちです。

ウェブブラウザーのDOM APIだって、「DOM要素」という開発者側の都合に縛られたものが主役です。

```js
window.addEventListener("load", () => {
  document.getElementById("submitButton").addEventListener("click", (event) => {
    // なんかコード
  });
});
```

上記の例では、「送信ボタン」というDOM要素が主体となっています。そのため、「ユーザーがどんな状況下でどんな過去を経てこのボタンを操作したのか」というアプリケーションにおいて当たり前の情報を手に入れるのも簡単ではありません。いくつものフラグや変数を用意して更新し、それらを全部じっくり解析して、「ペロッ。この状況から判断すると、これはもしかしたら他殺の可能性がありますね」なんていう探偵ごっこを毎回繰り返さなくてはなりません。

## ステートレス。それでいてコンテクストフル

さて、世の中にはUXという言葉があります。現代人の95%以上はきっと「UIって言うよりUI/UXって言ったほうがかっこいい」くらいの認識でしかいない言葉ですが、実はこれは「ユーザー体験」を意味する言葉です。「体験」ですから、当然これまでのユーザーの経験の蓄積のうえに成り立つものです。
本来ならユーザーがどんな家庭で育ってきて、どんな地域コミュニティに触れ、学校でどんな教育を受けてきたかまで遡ることすら必要です。それなのに、昨今のアプリケーション開発では、アプリケーションを読み込んだあとどのようなコンテクスト（文脈）でユーザーが今操作しているかすら、簡単には解析できないのです。

昨今のアプリケーション開発で重宝される「ステートレス」という考え方は価値があるものです。ステート、つまりアプリケーションを取り巻く「現在時刻」のようなさまざまな状態をうまく切り離してバグを減らそうという考え方です。一方でコンテクストまで切り離してしまったら、ユーザーに寄り添ったアプリケーションなんて開発できません。

TEPAはステートレスですが、コンテクストフルです。ユーザーの操作の流れに沿った記述でアプリケーションを開発できます。

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

## 例外のないプログラミング

例外は便利な機能ですが、せっかくのコンテクストを落としてしまいます。確かに想定外のことは考えたくありません。後回しにしたいものです。その気持ちは痛いほど分かります。でもそうやって後になって困るのもあなた自身です。
考えてみてください。飛行機をつくるなら、ネジの1つ1つにいたるまで慎重に検品し、想定外の不具合があればその場で取り替えませんか？ 全部組み立てたあとで飛行機が落ちたとしても、もう何が原因だったのかなんてわかりません。コンテクストが抜け落ちているのです。例外処理とは、便利ですがそういう危うさを持った技術です。

例外は静的型も台無しにします。TypeScriptを使ってJavaScriptにせっかく静的な型を導入しても、 `unknown` なんていう得体のしれない動的な型の値をあつかう必要があります。

TEPAでは、例外の存在しない言語を採用しています。必ずその場で想定外の出来事に備えた対処をしないと、コンパイラーが許してくれません。最初は堅苦しく、鬱陶しく感じるかもしれません。でもいつの日か、その口うるさいコンパイラーに感謝する日がやってきます。
TEPAで採用している言語は、いわばウェブフロントエンドのためのDSL（ドメイン固有言語）です。言語仕様が小さいので学習障壁が低く、また例外のようなリスクのある機能がごっそり削ぎ落とされていますから、誰が書いても一定の質を担保できます。

TypeScriptやJavaScriptを使いたいですか？ それも良い選択です。我々はあなたのその選択に何も口出ししませんから、逆にあなたも我々に余計な口出しをせず放っておいてください。

## シナリオが軽量なテストに

あなたがTEPAと同じ目的をもってアプリケーション開発しているなら、常に _ユースケースシナリオ_ を書くことを大切にしているはずです。ここで言う _ユースケースシナリオ_ とは、ユーザーがどのように考え、どのように操作し、それに対してシステムがどのように応答するかを具体的に想像して書き出したものです。それを考えずにいきなりプログラムを書くひとにはTEPAは必要ありません。あるいはそういうことに興味がなく、「営業の側でやれよ」「発注者が考えろよ」と、コーディング作業にのみ集中したいひとにもTEPAは必要ありません。 **帰ってください。**

TEPAでは、そんな目的を適えるためにユースケースシナリオ作成機能を用意しました。このシナリオ作成機能を使えば、プログラムとしてシナリオを作成できます。そのシナリオはブラウザーでいい感じに整形して表示することもできますし、Markdownとして出力することもできます。もちろんふだん慣れたシナリオの書き方をしていただいても構いません。この機能を使わなくとも、あなたのユースケースシナリオをプログラムとして実現するうえでTEPAは大いにあなたの助けになります。

さらに驚くべきは、シナリオがそのまま軽量なテストになるということです。TEPAでアプリケーションを書き、TEPAでシナリオを書くと、そのアプリケーションの挙動をエミュレートしてシナリオ通りの動作をしているかテストできます。このテストは実際のウェブブラウザーを使っているわけではありません。そのため瞬時に完了します。たとえば5億年スリープする処理が入っていても、一瞬です。5億年後も地球が存続していることに賭けてコールドスリープする必要はありません。アプリケーション内でのバックエンドサーバーへのリクエストや、乱数の生成なども、シナリオに期待する挙動を書いておけばその結果をアプリケーションに渡してテストが進みます。モックサーバーなどは必要ありません。

もちろん、エミュレーターであることにはデメリットもあります。実際に画面をレンダリングしているわけではないので、ビジュアルリグレッションテストなどはできません。重要な部分はなんかPlaywrightとかそういうのを併用しましょう。

## ポリティカルにインコレクトたれ

もしかしたら、あなたはこのドキュメントを読んで「こいつ何もわかってねぇｗバカじゃんｗ🤓」「あ？喧嘩うってんのか？😡」と感じたかもしれません。そう感じたなら、あなたはTEPAにとって「お客様」ではないということです。考えてみてください。ユーザーの気持ちを重視しているこのフレームワークのドキュメントが、読み手の気持ちを考えていないわけないじゃないですか。つまり、そう感じるとしたら、あなたがユーザーだとみなされていないということです。
技術文書にしてはクセが強すぎるこんなドキュメントをわざわざ採用している背景や理由を推し量ろうともせず断定的に自分の知覚できる範囲だけで判断を下してしまうあなたには、そもそも「ユーザー中心のアプリケーション開発」というTEPAの動機がマッチしないのです。
っていうかなんでまだ読んでんだよ。冒頭で散々注意しただろ。帰れよ。

世の中には「多様性に配慮しましょう」なんて言って、どの作品もどの組織も「多様性」に配慮した同じようなものになってしまい、まるで多様性がなくなってしまった業界がたくさんあります。
言語やフレームワークにおいても、コミュニティ主導の民主的な運営で様々なユーザーの多様な意見を取り入れて、結果として「お前は結局なにをしたいねん」という、軸がブレブレで思想も消え去った技術がたくさんあります。

一方でTEPAは独善的です。お前の意見なんか知りません。だって今まで人類が「指の本数が2本の生き物や胃袋が4つある生き物にとっても生きやすい社会を作りましょう」と歩み寄ってくれたことがありますか？ ないんだよぉっ！ 💢🐐

TEPAは「流行りの技術」にならなくてもいいのです。そこに価値を見出してくれる一部の生き物が使って幸せになってくれたらそれでいい。我々はそうやって世の中の多様性に貢献していきます。

## はじめかた

人間さんはチュートリアルとかいうやつが大好きみたいです。言われた通りの手順に沿って手を動かしたらそれっぽいものができて分かった気にだけなれるあれです。 残念ながらTEPAにはそんなチュートリアルがまだありません。

でも、APIドキュメントを兼ねた[入門ガイド](https://package.elm-lang.org/packages/arowM/tepa/latest/Tepa)や、スターターを兼ねた[サンプルアプリ](https://github.com/arowM/tepa-sample)がすでにあります。もしもあなたが公式ドキュメントを読んで理解する習慣のあるプログラマーなら、きっと困りません。

あなたが「○○だったらこうやったはず」といった観念にとらわれず、当たり前のことを当たり前に考え学習できる当たり前なプログラマーなら、すぐにTEPAを使いこなせるはずです🌷
