digits ← "0"‿"1"‿"2"‿"3"‿"4"‿"5"‿"6"‿"7"‿"8"‿"9"
words ← "zero"‿"one"‿"two"‿"three"‿"four"‿"five"‿"six"‿"seven"‿"eight"‿"nine"
numbers ←  ((•ParseFloat (⊑•args)) × 10) ↑ (digits ∾ words)
input ← •file.Lines (1⊑•args)

_index ← { ⊑(/𝔽´⍷⊢) 𝕩 }
_ofRow ← { 𝔽 _index (𝔽´¨𝕩) }
NumberFromRow ← {
  indices ← (/⍷)⟜𝕩¨numbers
  min ← 10|(⌊ _ofRow indices)
  max ← 10|(⌈ _ofRow indices)
  (min×10)+max
}

•Show +´NumberFromRow¨input
