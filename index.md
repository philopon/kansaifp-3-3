手元資料を用意しました!
--

1. ダウンロードして、

    curl -LO https://raw.githubusercontent.com/philopon/kansaifp-3-3/gh-pages/kansaifp3-3_type-level-programing.ipynb

2. tmpnb.orgを開いて、

    open "https://tmpnb.org"

3. ダウンロードしたファイルをドロップしてUpload

4. Uploadしたファイルをクリックして開く

スライドと同じ内容を書いてあるので、いろいろ評価(Shift-Enterとか)しながら見るとわかりやすいかも?

================================================================================


たのしい型レベルプログラミング
--
@philomel202

================================================================================

自己紹介
==
* [@philomel202](https://twitter.com/philomel202)([<i class="fa fa-github"></i>philopon](https://github.com/philopon))
* 大阪大学大学院 薬学研究科 D1

    * 定量的構造-活性相関(QSAR)
    * 分子シミュレーション

* Haskell大好き人間

    * [apiary](https://hackage.haskell.org/package/apiary)(webフレームワーク)
    * [bytestring-read](https://hackage.haskell.org/package/bytestring-read)(高速bytestring → 数値変換)


その他いろいろ

================================================================================

## 型レベルプログラミング

--------------------------------------------------------------------------------

まえおき
==

--------------------------------------------------------------------------------

型を語るということの七つの大罪
--
[Seven deadly sins of talking about “types”](http://www.cl.cam.ac.uk/%7Esrk31/blog/2014/10/07/) ([邦訳](http://qiita.com/GarbageUtilitas/items/77b17fab3cf2d53148fb))

--------------------------------------------------------------------------------

型レベルプログラミングを良いものであるかのように提示すること
--
およそ正気の人間なら、あるプログラミング言語をベースレベルと型レベルというはっきりと別々の断片へと二分することが望ましいのだ、などとは論じたりできないはずです。

--------------------------------------------------------------------------------

<h2>
＿人人人人人人人人人人人人＿  
＞　およそ正気の人間なら　＜  
￣Y^Y^Y^Y^Y^Y^Y^Y^Y^Y^Y￣
</h2>

--------------------------------------------------------------------------------

# 正気をなくそう!!!!!!!!

--------------------------------------------------------------------------------

正気をなくした人間
--

[![No sane person](img/no-sane-person.png)](https://twitter.com/philomel202/status/549367872103784448)

================================================================================

## 型レベルプログラミングの
## 豊かな世界

--------------------------------------------------------------------------------

型に情報を保持
--
例: コマンドラインオプションパーザ

optparse-declarative-0.3.0

<pre class="haskell highlight">
-- 型にコマンドラインオプションの情報を保持させる
greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Arg "NAME" String
      -> Cmd "Greeting command" ()

<span class="fragment">-- 値ではgetでオプションを取得して使える
greet (get -> msg) (get -> name) =
    liftIO $ putStrLn $ msg ++ ", " ++ name ++ "!"</span>

<span class="fragment">-- あとはrun_に与えるだけ
main :: IO ()
main = run_ greet</span>
</pre>

--------------------------------------------------------------------------------

型安全な辞書でルートパラメータを取得

例: webフレームワーク

apiary-1.4.3

<pre class="haskell highlight">
main :: IO ()
main = runApiary (run 3000) def $ do

    -- /hello/:name に対するルーテング
    <span class="fragment">path "hello" .</span>
        <span class="fragment">fetch (Proxy :: Proxy ("name" := Text)) Nothing .
        <span class="fragment">action $ do
            -- nameキーが存在していないとエラー
            -- 加えてnameはもともとText型に変換済み
            name &lt- param (Proxy :: Proxy "name")</span>
            <span class="fragment">text $ "Hello, " &lt&gt name</span>

    <span class="fragment">-- 流石にダルいのでQQを使える
    [capture|/hello2/name::Text|] . action $ do
        name &lt- param [key|name|]
        text $ "Hello, " &lt&gt name</span>
</pre>

--------------------------------------------------------------------------------

## ライブラリ開発者が苦しめば、ユーザーは美味しい所をいただける!!!!

--------------------------------------------------------------------------------

# その上たのしい!!!!!!!!

--------------------------------------------------------------------------------

# はじめよう、型レベルプログラミング!!!!!!

================================================================================

# はじめる前に

--------------------------------------------------------------------------------

型?
==

"データ型（データがた、data type）とは、コンピュータにおけるデータの扱いに関する形式のことである。データタイプとも。データ型は、プログラミングなどにおいて変数そのものや、その中に代入されるオブジェクトや値が対象となる。" - [データ型 - Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%87%E3%83%BC%E3%82%BF%E5%9E%8B)


--------------------------------------------------------------------------------

# なるほどわからん

--------------------------------------------------------------------------------

型?
==
よくわからないので、Haskellで型の名前空間に属するものとします(投げやり

<pre class="haskell highlight">
func :: (ここに書けるものは型)
func = undefined
</pre>

<pre class="haskell highlight">
Prelude> :type (!!)
<span class="fragment">(!!) :: [a] -> Int -> a -- :: の右は型。</span>

Prelude> :type (+)
<span class="fragment">(+) :: Num a => a -> a -> a -- Numなどの制約も型!</span>
</pre>

--------------------------------------------------------------------------------

kind?
==
型の型。基本的には`*`と`Constraint`の2種類

<pre class="haskell highlight">
Prelude> :kind Int
<span class="fragment">Int :: *</span>

Prelude> :kind (->)
<span class="fragment">(->) :: * -> * -> *</span>

Prelude> :kind Int -> Int
<span class="fragment">Int -> Int :: *</span>

Prelude> :kind Maybe
<span class="fragment">Maybe :: * -> *</span>

Prelude> :kind Maybe Int
<span class="fragment">Maybe Int :: *</span>

Prelude> :kind Num
<span class="fragment">Num :: * -> Constraint -- 制約はConstraint kind</span>
</pre>

-------------------------------------------------------------------------------

kindが違うとエラー
--

<pre class="haskell highlight">
<span>Prelude> :k Maybe Either</span>
<span class="fragment">
&ltinteractive&gt:1:7:
    Expecting two more arguments to ‘Either’
    The first argument of ‘Maybe’ should have kind ‘*’,
      but ‘Either’ has kind ‘* -> * -> *’
    In a type in a GHCi command: Maybe Either</span>
</span></pre>

================================================================================

# part1. 型から値へ

型に情報を保持しよう

--------------------------------------------------------------------------------

Phantom type
==

値に出て来ない型変数

<pre class="haskell highlight">
-- aは値に出て来ない
data Foo a = Foo { getFoo :: Int }
</pre>

ここに保持したい情報を入れれば良い

--------------------------------------------------------------------------------

Boolean型の情報を保持してみる
--

<pre class="haskell highlight">
-- 型が違う必要があるので別々に定義
data On
data Off

<span class="fragment">-- フラグを値にするための型クラス
class KnownFlag a where
    flagVal :: Foo a -> Bool

instance KnownFlag On where
    flagVal _ = True

instance KnownFlag Off where
    flagVal _ = False
</span>
<span class="fragment">-- 型からフラグを読みとって使用する
testFoo :: KnownFlag a => Foo a -> String
testFoo foo =
    (if flagVal foo then "On:" else "Off:") ++
    show (getFoo foo)</span>
</pre>

--------------------------------------------------------------------------------

つかってみよう
--

<pre class="haskell highlight">
*Main> testFoo (Foo 12 :: Foo On)
"On:12"
*Main> testFoo (Foo 12 :: Foo Off)
"Off:12"
</pre>

型から情報を読み取れた!!

--------------------------------------------------------------------------------

これでok?
--

`KnownFlag`は`Foo`型以外にも使いたい……

<pre class="haskell highlight">
class KnownFlag a where
    flagVal :: <span class="fragment fade-out highlight-code" data-fragment-index="1">Foo</span> a -> Bool -- Foo aをaに

instance KnownFlag On where
    flagVal _ = True

instance KnownFlag Off where
    flagVal _ = False

testFoo :: KnownFlag a => Foo a -> String
testFoo foo =
    (if flagVal (undefined :: <span class="fragment highlight-code" data-fragment-index="3">forall a.</span> a) then "On:" else "Off:") ++
    show (getFoo foo)</span>
</pre>

<div class="fragment" data-fragment-index="2">

<pre class="haskell highlight">
a.hs:17:9:
    Could not deduce (KnownFlag a0) arising from a use of ‘flagVal’
    from the context (KnownFlag a)
    (省略)
</pre>

2つのaが同じじゃない!!!つらい!!!
</div>

--------------------------------------------------------------------------------

ScopedTypeVariables
--

<pre class="haskell highlight">
{-# LANGUAGE ScopedTypeVariables #-}

testFoo :: <span class="fragment highlight-code" data-fragment-index="1">forall a.</span> KnownFlag a => Foo a -> String
testFoo foo =
    (if flagVal (undefined :: <span class="fragment highlight-code fade-out" data-fragment-index="1">forall a. </span><span class="fragment highlight-red" data-fragment-index="1">a</span>) then "On:" else "Off:") ++
    show (getFoo foo)</span>
</pre>

<p class="fragment" data-fragment-index="2">`forall`を明示的に付ける事で、それ以下の定義では暗黙のforallが付かなくなる!</p>

<pre class="fragment haskell highlight" data-fragment-index="2">
*Main> testFoo (Foo 12 :: Foo On)
"On:12"
*Main> testFoo (Foo 12 :: Foo Off)
"Off:12"
</pre>

--------------------------------------------------------------------------------

これでok?
--

Fooの型変数にはOn/Off以外も許されてしまう……

つらい……

<pre class="haskell highlight">
fooOk :: Foo Bool
fooOk = Foo 3 -- これはvalidだが……
</pre>

<pre class="fragment haskell highlight" data-fragment-index="1">
*Main> :t testFoo fooOk -- 当然testFooに与えるとエラー

&ltinteractive&gt:33:1:
    No instance for (KnownFlag Bool) arising from a use of ‘testFoo’
    In the expression: testFoo (Foo 3 :: Foo Bool)
    In an equation for ‘it’: it = testFoo (Foo 3 :: Foo Bool)
</pre>

<span class="fragment" data-fragment-index="1">`fooOk`の存在は許したくない!</span>

--------------------------------------------------------------------------------

DataKinds
--

値を型に、型をKindに昇格する拡張

データ構築子の頭に'を付けることで昇格された型である事を表わす

<pre class="haskell highlight">
*Main> :kind 'True -- True"型"のKindは?
<span class="fragment">'True :: Bool</span>

*Main> :kind 'Nothing -- 型変数を取る型も昇格できる
<span class="fragment">'Nothing :: Maybe k</span>

*Main> :kind 'Just 'True
<span class="fragment">'Just 'True :: Maybe Bool</span>
</pre>

--------------------------------------------------------------------------------

PolyKinds
--
多相なKindを取る型を定義できるようになる拡張

<pre class="haskell highlight">
Prelude Data.Proxy> :m Data.Proxy

-- Proxyの定義は data Proxy t = Proxy
Prelude Data.Proxy> :k Proxy
Proxy :: k -> *

<span class="fragment">Prelude Data.Proxy> Proxy :: Proxy Int -- 普通の型(kindは*)も、
Proxy</span>

<span class="fragment">Prelude Data.Proxy> Proxy :: Proxy 'True -- 昇格した型(kindはBool)も、
Proxy</span>

<span class="fragment">Prelude Data.Proxy> Proxy :: Proxy Maybe -- 型変数を取る型(kindは* -> *)も扱える
Proxy</span>
</pre>

--------------------------------------------------------------------------------

完成!
--

<pre class="haskell highlight">
{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds #-}
import Data.Proxy

data Foo (a :: Bool) = Foo { getFoo :: Int } -- 最早On/Offは不要

<span class="fragment" data-fragment-index="1">class KnownBool (a :: Bool) where
    boolVal :: proxy a -> Bool -- Proxy型である必要がないなら多相にしておくと便利

instance KnownBool 'True where
    boolVal _ = True

instance KnownBool 'False where
    boolVal _ = False</span>

<span class="fragment" data-fragment-index="2">testFoo :: forall a. KnownBool a => Foo a -> String
testFoo foo =
    (if <span class="fragment highlight-red" data-fragment-index="4">boolVal (Proxy :: Proxy a)</span> then "On:" else "Off:") ++
    show (getFoo foo)</span>

<span class="fragment" data-fragment-index="3">testFoo' :: KnownBool a => Foo a -> String -- 同じ動作
testFoo' foo = (if <span class="fragment highlight-red" data-fragment-index="4">boolVal foo</span> then "On:" else "Off:") ++ show (getFoo foo)</span>
</pre>

--------------------------------------------------------------------------------

型レベルリテラル
--

型に自然数、文字列を使用できる!

<pre class="haskell highlight">
Prelude> :set -XDataKinds
Prelude> :m GHC.TypeLits Data.Proxy

<span class="fragment">Prelude GHC.TypeLits Data.Proxy> :kind 3
3 :: Nat</span>

<span class="fragment">Prelude GHC.TypeLits Data.Proxy> :kind "foo"
"foo" :: Symbol</span>
</pre>

<pre class="fragment haskell highlight">
import GHC.TypeLits

typeLitTest :: (KnownNat n, KnownSymbol s) => proxyN n -> proxyS s -> [String]
typeLitTest nat sym = replicate
    (fromIntegral $ natVal nat) -- natValでInteger型の値に
    (symbolVal sym) -- symbolValでString型の値に
</pre>

<pre class="fragment haskell highlight">
*Main> typeLitTest (Proxy :: Proxy 5) (Proxy :: Proxy "foo")
["foo","foo","foo","foo","foo"]
</pre>

================================================================================

# part2. 型族

型同士の関係

--------------------------------------------------------------------------------

型族?
==

型族を使えば型関数を実現できる!

型レベル自然数には基本的な演算が定義されている

ghciの:kind!で評価できる

<pre class="haskell highlight">
Prelude> :m GHC.TypeLits
Prelude GHC.TypeLits> :set -XDataKinds -XTypeOperators

-- 関数+(値レベル)はInt 型  の値とInt 型  の値を取ってInt 型  の値を返す
Prelude GHC.TypeLits> :type  2 + (3::Int)
2 + (3::Int) :: Int

-- 型族+(型レベル)はNat kindの型とNat kindの型を取ってNat kindの型を返す
Prelude GHC.TypeLits> :kind! 2 + 3
2 + 3 :: Nat
= 5
</pre>

--------------------------------------------------------------------------------

長さ付きリスト
==
"型"に長さの情報が入ったリスト

当然、コンパイル時に長さが決まっている必要があるが、
型安全なhead/tailとか使えて便利

<pre class="haskell highlight">
-- 疑似コード
Prelude> :type Nil
Vector 0 a

Prelude> :type "foo" :- Nil
Vector 1 String

Prelude> vhead Nil
(コンパイル時エラー)

Prelude> vhead $ "foo" :- Nil
"foo"
</pre>

--------------------------------------------------------------------------------

スマートコンストラクタ?
--
Haskell98のdata宣言ではデータ構築子を使用して構築される型については言及できない

言及したい時はスマートコンストラクタとか使用してた

<pre class="haskell highlight">
-- よくある例
data Expr a
    = I Int                  -- Int  -> Expr Int
    | B Bool                 -- Bool -> Expr Bool
    | Add (Expr a) (Expr a)  -- Expr Int -> Expr Int -> Expr Int
    | Eq (Expr a) (Expr a)   -- Expr Int -> Expr Int -> Expr Bool

-- スマートコンストラクタ, 値レベル
i :: Int -> Expr Int
i = I

-- Iで構築されていても、型レベルではaがIntとは限らないのでコンパイルできない
eval :: Expr a -> a
eval (I i) = i
</pre>

--------------------------------------------------------------------------------

GADTs
--
GADTを使用すれば構築される型にも言及できる!

<pre class="haskell highlight">
{-# LANGUAGE GADTs, KindSignatures #-}
-- 同じ例(GADTs)

-- data 型 whereのあとに関数っぽく書く
-- 構築される型(赤)に言及できる!
data Expr a where
    I   :: Int  -> <span class="highlight-code">Expr Int</span>
    B   :: Bool -> <span class="highlight-code">Expr Bool</span>
    Add :: Expr Int -> Expr Int -> <span class="highlight-code">Expr Int</span>
    Eq  :: Expr Int -> Expr Int -> <span class="highlight-code">Expr Bool</span>

-- KindSignaturesを使って書くとツウっぽいかも?
-- data Expr :: * -> * where
--     I   :: Int  -> Expr Int
--     -- 以下省略

-- Iで構築されていれば、型レベルでもaはIntなのでコンパイルできる
eval :: Expr a -> a
eval (I i) = i
</pre>



--------------------------------------------------------------------------------

型レベル自然数リテラルで
--

<pre class="haskell highlight">
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, GADTs #-}
import GHC.TypeLits

data Vector :: Nat -> * -> * where
    Nil  :: Vector 0 a
    (:-) :: a -> Vector n a -> Vector (n + 1) a

infixr 5 :-
</pre>

<pre class="haskell highlight">
*Main> :t Nil
Nil :: Vector 0 a

*Main> :t (1::Int) :- Nil
(1::Int) :- Nil :: Vector (0 + 1) Int

*Main> :t 2 :- (1::Int) :- Nil
2 :- (1::Int) :- Nil :: Vector (1 + 1) Int
</pre>

いけてるっぽい?

--------------------------------------------------------------------------------

いけてない
--

tailを定義しようとすると……

<pre class="haskell highlight">
vtail :: Vector (n + 1) a -> Vector n a
vtail (_ :- b) = b
</pre>

<pre class="fragment haskell highlight">
a.hs:12:17:
    Could not deduce (n1 ~ n)
    from the context ((n + 1) ~ (n1 + 1))
    (省略)
</pre>

--------------------------------------------------------------------------------

ならば定義を変える!

<pre class="haskell highlight">
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, GADTs #-}

import GHC.TypeLits

data Vector :: Nat -> * -> * where
    Nil  :: Vector 0 a
    (:-) :: a -> Vector <span class="fragment" data-fragment-index="1">(</span>n<span class="fragment" data-fragment-index="1"> - 1)</span> a -> Vector <span class="fragment fade-out" data-fragment-index="1">(</span>n<span class="fragment fade-out" data-fragment-index="1"> + 1)</span> a

infixr 5 :-
</pre>

<pre class="haskell highlight">
*Main> :t Nil
Nil :: Vector 0 a

*Main> :t (1::Int) :- Nil
(1::Int) :- Nil :: Vector 1 Int

*Main> :t 2 :- (1::Int) :- Nil
2 :- (1::Int) :- Nil :: Vector 2 Int
</pre>

--------------------------------------------------------------------------------

やっぱりダメ

<pre class="haskell highlight">
vtail :: Vector (n + 1) a -> Vector n a
vtail (_ :- b) = b
</pre>

<pre class="haskell highlight">
a.hs:12:18:
    Couldn't match type ‘n’ with <span class="fragment highlight-red" data-fragment-index="1">‘(n + 1) - 1’</span>
</pre>

<p class="fragment" data-fragment-index="1">計算ができてない……つらい……</p>
<p class="fragment">型レベル自然数リテラルでは各数字間に関係が無いのが原因</p>

Note:
TypeLits https://github.com/ghc/ghc/blob/0aaea5b8345fc4b061b97df8bcaa7c6b07594719/compiler/typecheck/TcInteract.hs#L1639-1673

--------------------------------------------------------------------------------

ペアノ数
--

みんな大好きペアノ数

* 自然数 Z(Zの前に自然数は存在しない)
* 任意の自然数nの後者 S(n)

これで自然数を表わす

<pre class="haskell highlight">
data Nat = Z | S Nat

type N0 = 'Z
type N1 = 'S 'Z
type N2 = 'S N1
type N3 = 'S N2
</pre>

--------------------------------------------------------------------------------

ペアノ数で
--

<pre class="haskell highlight">
{-# LANGUAGE StandaloneDeriving #-}
data Vector :: Nat -> * -> * where
    Nil  :: Vector 'Z a
    (:-) :: a -> Vector n a -> Vector ('S n) a

deriving instance Show a => Show (Vector n a)
infixr 5 :-

vtail :: Vector ('S n) a -> Vector n a
vtail (_ :- b) = b
</pre>

<pre class="haskell highlight">
*Main> :t vtail Nil

<span class="fragment">&ltinteractive&gt:7:7:
    Couldn't match type ‘'Z’ with ‘'S n’
    (省略)</span>

*Main> vtail (1 :- (2 :- Nil))
<span class="fragment">2 :- Nil</span>
</pre>

<p class="fragment">やったね!!!</p>

--------------------------------------------------------------------------------

(++)
--

サクサクと(++)も定義しましょう!

<pre class="haskell highlight">
(+:+) :: Vector x a -> Vector y a -> Vector <span class="fragment highlight-red" data-fragment-index="1">???</span> a
Nil       +:+ ys = ys
(x :- xs) +:+ ys = x :- xs +:+ ys
</pre>

<span class="fragment" data-fragment-index="1">... ?</span>

--------------------------------------------------------------------------------

型族を定義する
--
openな型族

<pre class="haskell highlight">
type family (a :: Nat) + (b :: Nat) :: Nat

type instance 'Z   + b = b
type instance 'S a + b = 'S (a + b)

infixl 6 + -- 結合性、優先度も普通に設定できる
</pre>

* 後からtype instanceを追加できる
* <span class="fragment" data-fragment-index="1">重複するinstanceは定義できない</span>

<pre class="fragment haskell highlight" data-fragment-index="1">
type family   Fraction a -- 小数を表わす型の仮数部を十分に表わせられる型
type instance Fraction Float  = Word32  -- 仮数部 23bit
type instance Fraction Double = Word64  -- 仮数部 53bit
type instance Fraction a      = Integer -- デフォルトでは∞にしたいけれど……
</pre>

<pre class="fragment haskell highlight" data-fragment-index="1">
a.hs:16:15:
    Conflicting family instance declarations:
      Fraction Float -- Defined at a.hs:16:15
      (省略)
</pre>

--------------------------------------------------------------------------------

closedな型族
--

<pre class="haskell highlight">
type family a + b where
  'Z   + b = b
  'S a + b = 'S (a + b)
</pre>

* あとからtype instanceを追加できない
* 重複するinstanceも定義できる(上が優先)

型レベルプログラミング向き!!

--------------------------------------------------------------------------------

(++)
--

<pre class="haskell highlight">
type family a + b where -- 再掲
  'Z   + b = b
  'S a + b = 'S (a + b)
</pre>

<pre class="fragment haskell highlight" data-fragment-index="1">
*Main> :kind! 'S ('S 'Z) + 'S ( 'S ('S 'Z)) -- 2 + 3
'S ('S 'Z) + 'S ( 'S ('S 'Z)) :: Nat
= 'S ('S ('S ('S ('S 'Z)))) -- 5
</pre>

<p class="fragment" data-fragment-index="2">これを使えば問題なく++を定義できる</p>

<pre class="fragment haskell highlight" data-fragment-index="2">
(+:+) :: Vector x a -> Vector y a -> Vector (x + y) a
Nil       +:+ ys = ys
(x :- xs) +:+ ys = x :- (xs +:+ ys)
</pre>

--------------------------------------------------------------------------------

reverse
--

baseの定義どおりreverseを定義してみます

<pre class="haskell highlight">
reverse :: Vector n a -> Vector n a
reverse l = rev l Nil
  where
    rev :: Vector k a -> Vector l a -> Vector (k + l) a
    rev Nil       a = a
    rev (x :- xs) a = rev xs (x :- a)
</pre>

<pre class="fragment haskell highlight" data-fragment-index="1">
a.hs:33:13:
    Couldn't match type <span class="highlight-code">‘n’ with ‘n + 'Z’</span>
    (省略)

a.hs:37:23:
    Could not deduce <span class="highlight-code">((n1 + 'S l) ~ 'S (n1 + l))</span>
    (省略)
</pre>

<p class="fragment" data-fragment-index="1">やっぱりダメじゃないですかーー!やだー!!!</p>

--------------------------------------------------------------------------------

# bad end...
part3へつづく

================================================================================

part3. 等しさ
==
そして証明へ

--------------------------------------------------------------------------------

再掲(reverse)
--

<pre class="haskell highlight">
reverse :: Vector n a -> Vector n a
reverse l = rev l Nil
  where
    rev :: Vector k a -> Vector l a -> Vector (k + l) a
    rev Nil       a = a
    rev (x :- xs) a = rev xs (x :- a)
</pre>

<pre class="haskell highlight">
a.hs:33:13:
    Couldn't match type <span class="highlight-code">‘n’ with ‘n + 'Z’</span>
    (省略)

a.hs:37:23:
    Could not deduce <span class="highlight-code">((n1 + 'S l) ~ 'S (n1 + l))</span>
    (省略)
</pre>

`n ~ n + 'Z`、`(n1 + 'S l) ~ 'S (n1 + l)`が解らない

<h2 class="fragment">教えてあげよう!</h2>

--------------------------------------------------------------------------------

Data.Type.Equality
--

ある2つの型が同じ型である事を示すためのモジュール

<pre class="haskell highlight">
data a :~: b where
    Refl :: a :~: a -- Reflで構築するとaとbが同じである事を示せる
</pre>

<p class="fragment" data-fragment-index="1">
GADTsでデータ構築子に対応する型を教られるので
</p>

<pre class="fragment haskell highlight" data-fragment-index="1">
sym Refl -- パターンマッチさせるとaとbが同じだとわかる
    = Refl
sym :: (a :~: b) -> (b :~: a) -- ので対称律もわかる
</pre>

<p class="fragment" data-fragment-index="2">
同様にして
</p>

<pre class="fragment haskell highlight" data-fragment-index="2">
trans :: (a :~: b) -> (b :~: c) -> a :~: c 
castWith :: (a :~: b) -> a -> b 
apply :: (f :~: g) -> (a :~: b) -> f a :~: g b 
inner :: (f a :~: g b) -> a :~: b 
outer :: (f a :~: g b) -> f :~: g 
</pre>

<p class="fragment" data-fragment-index="2">
なども定義されている
</p>

--------------------------------------------------------------------------------

:~:上での演算
--
まずは同じ数を左から足しても同じ事を証明しましょう

<pre class="haskell highlight">
addL :: proxy k -> n :~: m -> k + n :~: k + m
addL _ Refl = Refl
</pre>

Reflをパターンマッチすれば n ~ m がわかるので、  
k + n ~ k + m もわかります!

かんたん!

--------------------------------------------------------------------------------

左単位元
--
足し算について、0が左右の単位元であることを証明します

<p class="fragment" data-fragment-index="1">
左単位元である事は足し算の定義
</p>

<pre class="fragment haskell highlight" data-fragment-index="1">
Z   + b = b -- そのまま
S a + b = S (a + b)
</pre>

<p class="fragment" data-fragment-index="1">
から明らかなので、
</p>

<pre class="fragment haskell highlight" data-fragment-index="2">
plusLZ :: proxy n -> 'Z + n :~: n
plusLZ _ = Refl
</pre>

<p class="fragment" data-fragment-index="2">
で証明できます
</p>

--------------------------------------------------------------------------------

右単位元?
--
右単位元は自明ではないので、

<pre class="haskell highlight">
plusRZ :: proxy n -> n + 'Z :~: n
plusRZ _ = Refl
</pre>

<pre class="fragment haskell highlight" data-fragment-index="1">
[1 of 1] Compiling List             ( List.hs, interpreted )

List.hs:45:15:
    Couldn't match type ‘n’ with ‘n + 'Z’
    (省略)
</pre>

<p class="fragment" data-fragment-index="1">
はエラーになります
</p>

--------------------------------------------------------------------------------

帰納法
--
右単位元である事(n + Z ~ n)は帰納法を使うと証明できます

ある自然数nについて、

* n = Z のとき、自明
* n = S n のとき、S (n + Z) ~ S (n)

<h2 class="fragment">
場合分け……?
</h2>

--------------------------------------------------------------------------------

シングルトン
--

ある型と1:1で対応する値

まずは自然数に対応するシングルトンを定義しましょう!

<pre class="fragment haskell highlight">
data SNat :: Nat -> * where
    SZ :: SNat 'Z               -- SZ   は 'Z   と対応
    SS :: SNat n -> SNat ('S n) -- SS n は 'S n と対応
</pre>

<p class="fragment">
シングルトン同士の足し算も定義しておきましょう
</p>

<pre class="fragment haskell highlight">
(.+) :: SNat x -> SNat y -> SNat (x + y)
SZ   .+ y = y
SS x .+ y = SS (x .+ y)

infixl 6 .+
</pre>

--------------------------------------------------------------------------------

右単位元
--

<pre class="haskell highlight">
plusRZ :: SNat n -> n + 'Z :~: n

-- n = Z のとき、Z + Z :~: Z で、左単位元と同じく自明
plusRZ SZ = Refl

-- n = S n のとき、S (n + Z) ~ S (n)
plusRZ (SS n) = apply Refl (plusRZ n) -- Reflの型は S :~: S
</pre>

これで型チェックにとおりました!

<p class="fragment" data-fragment-index="1">
残念ながら停止性は証明できないので、評価して確認しましょう。
</p>

<pre class="fragment haskell highlight" data-fragment-index="1">
*List> plusRZ SZ -- 停止すればok
Refl

*List> plusRZ (SS $ SS $ SZ)
Refl
</pre>

--------------------------------------------------------------------------------

結合法則
--
同じ様にして足し算の結合法則も証明できます

<pre class="haskell highlight">
plusAssoc :: SNat x -> SNat y -> SNat z -> (x + y) + z :~: x + (y + z)

<span class="fragment" data-fragment-index="1">-- x = 0 のとき、(0 + y) + z :~: 0 + (y + z), 左単位元が自明なのでこれも自明
plusAssoc  SZ    _ _ = Refl</span>

<span class="fragment" data-fragment-index="2">-- x = S x のとき、 S ((x + y) + z) :~: S (x + (y + z))
plusAssoc (SS x) y z = apply Refl (plusAssoc x y z)</span>
</pre>

<p class="fragment" data-fragment-index="3">
また、以下の補題も同様に証明しておきましょう
</pre>

<pre class="fragment haskell highlight" data-fragment-index="3">
sAndPlusOne :: SNat n -> 'S n :~: n + N1

<span class="fragment" data-fragment-index="4">-- n = 0 のとき、S Z :~: Z + S Z, 右単位元(ry
sAndPlusOne  SZ    = Refl</span>

<span class="fragment" data-fragment-index="5">-- n = S n のとき、 S (S n) :~: S (n + N1)
sAndPlusOne (SS n) = apply Refl (sAndPlusOne n)</span>
</pre>

--------------------------------------------------------------------------------

n + S m :~: S (n + m)
--
ちょっと複雑なので、ghcに聞きながら進めましょう!

Type Holesを使えばghcがその場所の型を教えてくれます

<pre class="haskell highlight">
plusSR :: SNat n -> SNat m -> n + 'S m :~: 'S (n + m)
plusSR n m =
    Refl
    <span class="fragment" data-fragment-index="1">`trans` addL n (sAndPlusOne m)</span>
    <span class="fragment" data-fragment-index="3">`trans` sym (plusAssoc n m (SS SZ))</span>
    <span class="fragment" data-fragment-index="5">`trans` sym (sAndPlusOne (n .+ m))</span>
    <span class="fragment fade-out" data-fragment-index="7">`trans` (undefined :: _)</span>
</pre>

<pre class="fragment haskell highlight" data-fragment-index="2">
List.hs:67:27:
    Found hole ‘_’ with type: <span class="highlight-code">(n + (m + N1))</span> :~: 'S (n + m)
</pre>

<pre class="fragment haskell highlight" data-fragment-index="4">
List.hs:68:27:
    Found hole ‘_’ with type: <span class="highlight-code">((n + m) + 'S 'Z)</span> :~: 'S (n + m)
</pre>

<pre class="fragment haskell highlight" data-fragment-index="6">
List.hs:70:27:
    Found hole ‘_’ with type: <span class="highlight-code">'S (n + m) :~: 'S (n + m)</span>
</pre>

Note:
1. 括弧内がsAndPlusOne

--------------------------------------------------------------------------------

reverseの証明(1)
--
reverseの証明のために、

* 長さ付きリストの長さをシングルトンで返す関数
* 自然数のEqualityをリストの長さのEqualityに持ち上げる関数

を定義しましょう

<pre class="haskell highlight">
<span class="fragment">sLength :: Vector n a -> SNat n
sLength  Nil      = SZ
sLength (_ :- as) = SS (sLength as)</span>

<span class="fragment">asVectorSize :: n :~: m -> Vector n a :~: Vector m a 
asVectorSize Refl = Refl</span>
</pre>

--------------------------------------------------------------------------------

reverseの証明(2)
--
あとはこれらとcastWithで型を教えてあげましょう!

<pre class="haskell highlight">
reverse :: Vector n a -> Vector n a
reverse l = <span class="fragment highlight-code" data-fragment-index="1">castWith (asVectorSize $ plusRZ (sLength l)) $</span> rev l Nil
  where
    rev :: Vector k a -> Vector l a -> Vector (k + l) a
    rev Nil       a = a
    rev (x :- xs) a =
        <span class="fragment highlight-code" data-fragment-index="2">castWith (asVectorSize $ plusSR (sLength xs) (sLength a)) $</span>
        rev xs (x :- a)
</pre>

<pre class="haskell highlight">
<span class="fragment fade-out" data-fragment-index="1">a.hs:33:13:
    Couldn't match type <span class="highlight-code">‘n’ with ‘n + 'Z’</span>
    (省略)</span>

<span class="fragment fade-out" data-fragment-index="2">a.hs:37:23:
    Could not deduce <span class="highlight-code">((n1 + 'S l) ~ 'S (n1 + l))</span>
    (省略)</span>
</pre>

<h2 class="fragment">完成!</h2>

================================================================================

# まとめ

--------------------------------------------------------------------------------

## 型レベルプログラミングを使えば、

## 楽しい事がたくさん!!

--------------------------------------------------------------------------------

# 苦しい事もたくさん!!!

--------------------------------------------------------------------------------

## Let's 苦しく楽しい型レベルプログラミング!

--------------------------------------------------------------------------------

紹介し切れなかった事
--
* 関数従属など型クラスを使った型レ
* 型レベルリスト
* reflection
* liquid haskell

その他色々

--------------------------------------------------------------------------------

<pre style="font-family: MS PGothic; font-size: 42px">
:::::::::::::::型　は　こ　　　　　　　　　　　 {::::::{
::::::::::::::::レ　て　の　　　　　　＿ ,－ｖ 　 ､::::::､
::::::::::::::::坂　し　　　　　　　_/rｧ　￣ヽｎ　 ヽ::::::ヽ
:::::::::::::::::を　な　　　　　-こヽ__）ヽ へフ -‐':::::::::::}
:::::::::::::::::よ  く　　 ／:::::::／／,　7′:::::::::::::::::::::／
::::＿ｎ＿ ： 遠　　 ､:::::::::ｰ' ／／-‐　 ば　の　よ　オ
:::｀ﾆｌ ｌﾆ　　い　　　ヽ:::::／／＼　　　  か　ぼ　う　 レ
::::｀ﾌ　＼:::::::::ヽ ＿_ ﾉ:::ｰ':::::::::::::ヽ　　 り　 り　や　は
／'´|_|`ﾆ＿::::::::::::::::::::::::::::::::::::::::::::::l　 だ　は　く
:::::::ノ'ｒ三７/::::::::::::::::::::::::::::::::::::::::::::::}　か　じ
::::::::`ﾌ, 匸/ｌ::::::::::::::::::::::::::::::::::::::::::::/　 ら　め
::::::￣´:::￣´:::::::::::::::::::::::::::::::::::::::／ 　 な　 た
</pre>

--------------------------------------------------------------------------------

参考
--
* Haskell - optparse-declarative: 宣言的な型レベルコマンドラインパーザー - Qiita
http://qiita.com/tanakh/items/b6ea4c65d8ed511ac98d
* GHC 7.4.1 の型レベル新機能を使い倒す 〜GADTs、型族 と DataKinds、ConstraintKinds の円環〜 - konn-san.com
http://konn-san.com/articles/2012-06-06-promoted-types-and-list-arguments.html
* type-natural: Type-level natural and proofs of their properties. | Hackage
https://hackage.haskell.org/package/type-natural
