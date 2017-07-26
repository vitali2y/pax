import itt from 'itt'
import { square } from './math'

console.log(itt.range(10).map(square).join(' '))
