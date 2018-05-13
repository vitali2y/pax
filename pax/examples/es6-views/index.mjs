import { counter, incCounter } from './counter'

console.log(counter) // 3
incCounter()
console.log(counter) // 4

++counter // TypeError
