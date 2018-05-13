import even from './even'
export default function odd(n) {
  return n != 0 && even(n - 1)
}
