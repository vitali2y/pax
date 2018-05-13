const itt = require('itt')
const math = require('./math')

console.log(itt.range(10).map(math.square).join(' '))
