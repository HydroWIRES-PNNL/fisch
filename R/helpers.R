
# version of %in% that evades machine tolerance issue
`%.in%` = function(a, b, eps = .Machine$double.eps ^ 0.5) {
  any(abs(b-a) <= eps)
}
