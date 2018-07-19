import CONST, {c1, i1, counter, incCounter, SOMETHING} from './outer'

console.log(CONST)
console.log(SOMETHING)

console.log(counter) // 3
incCounter()
console.log(counter) // 4

console.log(c1) // 1
i1()
console.log(c1) // 2

++counter // TypeError
