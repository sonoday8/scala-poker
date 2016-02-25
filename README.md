
[Haskellでポーカーを作ろう](http://tune.hateblo.jp/entry/2015/05/12/023112)

上記リンクのサイトを参考に作成したHaskellでのポーカーゲームをScalaの勉強しながらScalaへ移植してます。

HaskellとScalaでの関数の違いは、以下のサイトを参考にしております。

1. [リスト操作関数早見表](http://techlog.mvrck.co.jp/entry/underscore-func/)

ソースディレクトリ：https://github.com/sonoday8/scala-poker/tree/master/src/main/scala/com/example

実行方法
activator run

以下のような表示になるので
 Multiple main classes detected, select one to run:

  [1] com.example.Hello
  [2] com.example.Main

 Enter number: 2
2を選ぶ

TODO: 
　１．do記法部分をパターンマッチで書いてしまっているので、forに変更

