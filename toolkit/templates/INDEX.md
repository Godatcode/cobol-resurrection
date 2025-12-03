# 📚 Template Index

Complete reference for all available templates in the @necro-bridge/core toolkit.

## 🎯 Quick Navigation

- **Getting Started:** [QUICKSTART.md](./QUICKSTART.md)
- **Full Documentation:** [README.md](./README.md)
- **Directory Setup:** [DIRECTORY-STRUCTURE.md](./DIRECTORY-STRUCTURE.md)
- **Deployment:** [DEPLOYMENT.md](./DEPLOYMENT.md)

## 📦 Template Files

### Core Infrastructure

| File | Purpose | Required |
|------|---------|----------|
| `LegacyBridge.js` | Abstract base class for all bridges | ✅ Yes |
| `universal-server.js` | Multi-language Express server | ⚠️ Optional |
| `package.json.template` | NPM package configuration | ✅ Yes |
| `test-bridge.js` | Test script for validating bridges | ⚠️ Recommended |

### Language-Specific Bridges

| File | Language | Year | Description |
|------|----------|------|-------------|
| `cobol-bridge.js` | COBOL | 1959 | Business-oriented calculations |
| `fortran-bridge.js` | FORTRAN | 1957 | Scientific computing |
| `pascal-bridge.js` | PASCAL | 1970 | Structured programming |
| `basic-bridge.js` | BASIC | 1983 | General-purpose computing |

### Documentation

| File | Purpose |
|------|---------|
| `README.md` | Complete template documentation |
| `QUICKSTART.md` | 5-minute setup guide |
| `DIRECTORY-STRUCTURE.md` | Recommended project layouts |
| `DEPLOYMENT.md` | Production deployment guide |
| `INDEX.md` | This file - template index |

## 🚀 Usage Paths

### Path 1: Single Language (Fastest)

**Time:** 5 minutes  
**Complexity:** ⭐ Easy

**Files Needed:**
1. `LegacyBridge.js`
2. `cobol-bridge.js` (or your language)
3. `package.json.template`

**Steps:**
1. Copy files to your project
2. Customize bridge configuration
3. Uncomment standalone server code
4. Run: `node cobol-bridge.js`

**Best For:**
- Quick prototypes
- Single legacy binary
- Learning the system

### Path 2: Multi-Language (Recommended)

**Time:** 15 minutes  
**Complexity:** ⭐⭐ Moderate

**Files Needed:**
1. `LegacyBridge.js`
2. All `*-bridge.js` files you need
3. `universal-server.js`
4. `package.json.template`
5. `test-bridge.js`

**Steps:**
1. Copy files to your project
2. Customize each bridge
3. Register bridges in universal-server.js
4. Run: `npm start`

**Best For:**
- Multiple legacy languages
- Production applications
- Scalable architecture

### Path 3: Production-Ready (Complete)

**Time:** 1 hour  
**Complexity:** ⭐⭐⭐ Advanced

**Files Needed:**
- All template files
- Additional: logging, monitoring, security

**Steps:**
1. Follow Path 2
2. Add security (API keys, rate limiting)
3. Add monitoring (logging, health checks)
4. Set up CI/CD
5. Deploy to production

**Best For:**
- Enterprise applications
- Public APIs
- Mission-critical systems

## 📖 Documentation Guide

### For Beginners

1. Start with [QUICKSTART.md](./QUICKSTART.md)
2. Follow the 5-minute setup
3. Test with your binary
4. Expand as needed

### For Developers

1. Read [README.md](./README.md) for complete documentation
2. Review [DIRECTORY-STRUCTURE.md](./DIRECTORY-STRUCTURE.md) for organization
3. Customize templates for your use case
4. Add tests and monitoring

### For DevOps

1. Review [DEPLOYMENT.md](./DEPLOYMENT.md) for deployment options
2. Set up CI/CD pipeline
3. Configure monitoring and alerts
4. Implement security best practices

## 🎨 Customization Examples

### Example 1: Custom Binary Location

```javascript
// In your bridge file
getBinaryPath() {
  return '/opt/legacy/binaries/mortgage';
}
```

### Example 2: Custom Output Format

```javascript
// In your bridge file
parseOutput(stdout) {
  // Match "TOTAL: $1,234.56"
  const resultPattern = /TOTAL:\s*\$?([\d,]+\.?\d*)/;
  const match = stdout.match(resultPattern);
  
  if (!match) {
    return { success: false, result: null, error: 'PARSE FAILED' };
  }
  
  // Remove commas and parse
  const result = parseFloat(match[1].replace(/,/g, ''));
  
  return { success: true, result: result, error: null };
}
```

### Example 3: Custom Validation

```javascript
// In your bridge file
validateParams(params) {
  // Call parent validation
  const baseValidation = super.validateParams(params);
  if (!baseValidation.valid) return baseValidation;
  
  // Add custom rules
  if (params.principal > 10000000) {
    return {
      valid: false,
      error: 'PRINCIPAL EXCEEDS $10M LIMIT'
    };
  }
  
  if (params.rate > 30) {
    return {
      valid: false,
      error: 'INTEREST RATE EXCEEDS 30% LIMIT'
    };
  }
  
  return { valid: true, error: null };
}
```

## 🔧 Common Modifications

### Add Logging

```javascript
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' })
  ]
});

// Use in bridge
async execute(params) {
  logger.info('Executing calculation', { params });
  // ... rest of code
}
```

### Add Caching

```javascript
const NodeCache = require('node-cache');
const cache = new NodeCache({ stdTTL: 600 });

async execute(params) {
  const cacheKey = JSON.stringify(params);
  const cached = cache.get(cacheKey);
  
  if (cached) {
    return { ...cached, cached: true };
  }
  
  const result = await super.execute(params);
  cache.set(cacheKey, result);
  
  return result;
}
```

### Add Retry Logic

```javascript
async executeWithRetry(params, maxRetries = 3) {
  for (let i = 0; i < maxRetries; i++) {
    try {
      return await this.execute(params);
    } catch (error) {
      if (i === maxRetries - 1) throw error;
      await new Promise(resolve => setTimeout(resolve, 1000 * (i + 1)));
    }
  }
}
```

## 📊 Template Comparison

| Feature | Single Language | Multi-Language | Production |
|---------|----------------|----------------|------------|
| Setup Time | 5 min | 15 min | 1 hour |
| Files Needed | 3 | 6+ | 10+ |
| Complexity | Low | Medium | High |
| Scalability | Limited | Good | Excellent |
| Security | Basic | Medium | Advanced |
| Monitoring | None | Basic | Comprehensive |
| Best For | Prototypes | Applications | Enterprise |

## 🎓 Learning Resources

### Tutorials

1. **Basic Setup:** Follow QUICKSTART.md
2. **Multi-Language:** See README.md examples
3. **Production:** Review DEPLOYMENT.md

### Example Projects

Check the main repository for complete working examples:
- COBOL mortgage calculator
- FORTRAN trajectory calculator
- PASCAL tax calculator
- BASIC interest calculator

### Video Guides (Coming Soon)

- Setting up your first bridge
- Deploying to production
- Advanced customization

## 🤝 Contributing

Want to improve these templates?

1. Test with your legacy language
2. Document your modifications
3. Submit improvements
4. Share your use case

## 📞 Support

- **Documentation:** Read the guides in this directory
- **Examples:** Check the main repository
- **Issues:** Report problems on GitHub
- **Community:** Share your projects!

## 🎯 Next Steps

1. **Choose your path** (Single, Multi, or Production)
2. **Copy the files** you need
3. **Customize** for your binary
4. **Test** thoroughly
5. **Deploy** to production
6. **Share** your success story!

## 📝 Checklist

### Before You Start
- [ ] Have a compiled legacy binary
- [ ] Know the binary's input format
- [ ] Know the binary's output format
- [ ] Have Node.js installed
- [ ] Have basic JavaScript knowledge

### Setup
- [ ] Copy template files
- [ ] Install dependencies
- [ ] Customize bridge configuration
- [ ] Update output parser
- [ ] Test locally

### Production
- [ ] Add security (API keys, rate limiting)
- [ ] Add monitoring (logging, health checks)
- [ ] Set up CI/CD
- [ ] Deploy to platform
- [ ] Test in production
- [ ] Monitor and maintain

## 🏆 Success Stories

Share your implementation! We'd love to hear how you're using these templates to resurrect legacy systems.

---

**Happy Resurrecting! 👻**

*Generated by @necro-bridge/core - Bringing vintage computing back to life*
