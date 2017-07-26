~function(global) {
  const Parcel = {}
  Parcel.baseRequire = typeof require !== "undefined" ? require : n => {
    throw new Error(`Could not resolve module name: ${n}`)
  }
  Parcel.modules = {}
  Parcel.files = {}
  Parcel.mains = {}
  Parcel.resolve = (base, then) => {
    base = base.split('/')
    base.shift()
    for (const p of then.split('/')) {
      if (p === '..') base.pop()
      else if (p !== '.') base.push(p)
    }
    return '/' + base.join('/')
  }
  Parcel.Module = function Module(filename, parent) {
    this.filename = filename
    this.id = filename
    this.loaded = false
    this.parent = parent
    this.children = []
    this.exports = {}
  }
  Parcel.makeRequire = self => {
    let parts
    const require = m => {
      let fn = self ? require.deps[m] : Parcel.main
      // if (fn === undefined) {
      //   const filename = require.resolve(m)
      //   fn = filename !== null ? Parcel.files[filename] : null
      // }
      if (fn == null) return Parcel.baseRequire(m)
      if (fn.module) return fn.module.exports
      const module = new Parcel.Module(fn.filename, self)
      fn.module = module
      module.require = Parcel.makeRequire(module)
      module.require.deps = fn.deps
      module.require.main = self ? self.require.main : module
      if (self) self.children.push(module)
      fn(module, module.exports, module.require)
      module.loaded = true
      return module.exports
    }
    require.deps = {}
    require.main = self
    require.module = m => {
      const result = require(m)
      if (!result.__esModule) result = {default: result}
      return result
    }
    // require.resolve = n => {
    //   if (!self) return n
    //   if (n[0] === '.' || n[0] === '/') {
    //     const p = resolvePath(n[0] === '.' ? Parcel.resolve(self.filename, '../'+n) : n)
    //     if (p) return p
    //   } else {
    //     if (!parts) {
    //       parts = self ? self.filename.split('/') : []
    //       parts.shift()
    //     }
    //     const p = parts.slice()
    //     while (p.length) {
    //       p.pop()
    //       if (p[p.length - 1] === 'node_modules') continue
    //       const r = resolvePath('/' + p.join('/') + '/node_modules/' + n)
    //       if (r) return r
    //     }
    //   }
    //   return null
    // }
    // const resolvePath = b => {
    //   const m = Parcel.mains[b]
    //   if (m) return m
    //   if (Parcel.files[b+'/index.js']) return b+'/index.js'
    //   if (Parcel.files[b+'/index.json']) return b+'/index.json'
    //   if (Parcel.files[b]) return b
    //   if (Parcel.files[b+'.js']) return b+'.js'
    //   if (Parcel.files[b+'.json']) return b+'.json'
    // }
    return require
  }
