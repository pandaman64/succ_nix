# 4/28
`not`に
```
not: true -> false ∪ false -> true
```
みたいな型がつくと情報が増えて素晴らしい。

しかし、この型を普通に解釈すると
```
[| true -> false ∪ false -> true |]
= [| true -> false |] ∪ [| false -> true |]
= { f: ∀x. f x →* v implies x ∈ {true} and v ∈ {false} } ∪ { f: ∀x. f x →* v implies x ∈ {false} and v ∈ {true} }
= ç ∪ { f: ∀x. f x →* v implies x = true and v = false }
```

この解釈の何が問題かというと、`not`という項が∪のどちらかに住んでいる項になってしまうため、型として正しくない。
一方で、
```
[| true -> false ∪ false -> true |]
= { f: ∀x. f x →* v implies ((x = true and v = false) or (x = false and v = true)) }
```
と∀を「くくり出す」形で解釈できると正確である。
しかし、この解釈では[| t |]がcompositionalでなくなってしまう…

対策として、とりあえずFunction unionは考えないようにする。
代わりに`A1 -> R1 ∪ A2 -> R2 = A1 ∪ A2 -> R1 ∪ R2`と潰してしまうことにしよう。当面。