# NPM Publication Guide for @necro-bridge/core

## ✅ Pre-Publication Checklist (COMPLETED)

- [x] **Package builds successfully** - TypeScript compilation completes without errors
- [x] **Package structure is correct** - All necessary files included in tarball
- [x] **Local installation works** - Package installs and CLI commands function properly
- [x] **Documentation is complete** - README.md, LICENSE, and other docs are present
- [x] **Version is set** - Currently at v1.0.0
- [x] **Dependencies are correct** - All required dependencies listed in package.json

## 📦 Package Information

- **Name**: `@necro-bridge/core`
- **Version**: `1.0.0`
- **Size**: 41.8 kB (packed), 188.5 kB (unpacked)
- **Files**: 48 total files
- **License**: MIT

## 🚀 Publication Steps

### Step 1: Create NPM Account (If Needed)

If you don't already have an NPM account:

1. Visit https://www.npmjs.com/signup
2. Create an account with:
   - Username
   - Email address
   - Password
3. Verify your email address

### Step 2: Login to NPM

```bash
npm login
```

You'll be prompted for:
- Username
- Password
- Email
- One-time password (if 2FA is enabled)

### Step 3: Verify Login

```bash
npm whoami
```

This should display your NPM username.

### Step 4: Publish to NPM Registry

From the `toolkit` directory:

```bash
cd toolkit
npm publish --access public
```

**Note**: The `--access public` flag is required for scoped packages (@necro-bridge/core) to be publicly accessible.

### Step 5: Verify Publication

After successful publication, verify at:
- https://www.npmjs.com/package/@necro-bridge/core

Or test installation:

```bash
npm install -g @necro-bridge/core
necro-bridge --version
```

## 🔐 Important Considerations

### Scoped Package Access

Since this is a scoped package (`@necro-bridge/core`), you must:
- Use `--access public` flag when publishing
- Or configure in package.json:
  ```json
  {
    "publishConfig": {
      "access": "public"
    }
  }
  ```

### Two-Factor Authentication (2FA)

If you have 2FA enabled on your NPM account:
- You'll need to provide a one-time password during `npm publish`
- Consider using `npm token create` for CI/CD pipelines

### Version Management

For future updates:

```bash
# Patch version (1.0.0 -> 1.0.1)
npm version patch

# Minor version (1.0.0 -> 1.1.0)
npm version minor

# Major version (1.0.0 -> 2.0.0)
npm version major

# Then publish
npm publish --access public
```

## 🧪 Post-Publication Testing

After publishing, test the package:

```bash
# Create a test directory
mkdir /tmp/necro-bridge-test
cd /tmp/necro-bridge-test

# Install from NPM
npm install -g @necro-bridge/core

# Test CLI commands
necro-bridge --version
necro-bridge --help
necro-bridge detect --help
necro-bridge init cobol --help
```

## 📊 Package Statistics

After publication, you can track:
- Download statistics: https://npm-stat.com/charts.html?package=@necro-bridge/core
- Package health: https://snyk.io/advisor/npm-package/@necro-bridge/core
- Bundle size: https://bundlephobia.com/package/@necro-bridge/core

## 🔄 Continuous Deployment

For automated publishing (optional):

1. **GitHub Actions** - Create `.github/workflows/publish.yml`:
   ```yaml
   name: Publish to NPM
   on:
     release:
       types: [created]
   jobs:
     publish:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         - uses: actions/setup-node@v3
           with:
             node-version: '18'
             registry-url: 'https://registry.npmjs.org'
         - run: npm ci
         - run: npm run build
         - run: npm publish --access public
           env:
             NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
   ```

2. **NPM Token** - Create automation token:
   ```bash
   npm token create --read-only=false
   ```
   Add to GitHub Secrets as `NPM_TOKEN`

## 🐛 Troubleshooting

### Error: "You must be logged in to publish packages"
**Solution**: Run `npm login` and authenticate

### Error: "You do not have permission to publish"
**Solution**: Ensure you're logged in with the correct account and have access to the @necro-bridge scope

### Error: "Package name too similar to existing package"
**Solution**: Choose a different package name or scope

### Error: "Version already exists"
**Solution**: Increment the version number using `npm version patch/minor/major`

## 📝 Maintenance Checklist

After publication:

- [ ] Update GitHub repository URL in package.json
- [ ] Create GitHub release matching NPM version
- [ ] Update CHANGELOG.md with release notes
- [ ] Announce on social media / developer communities
- [ ] Monitor for issues and bug reports
- [ ] Respond to NPM package questions

## 🎯 Success Criteria

The package is successfully published when:

1. ✅ Package appears on https://www.npmjs.com/package/@necro-bridge/core
2. ✅ Global installation works: `npm install -g @necro-bridge/core`
3. ✅ CLI commands execute: `necro-bridge --help`
4. ✅ Package can be imported: `require('@necro-bridge/core')`
5. ✅ Documentation is accessible on NPM page

## 🏆 Current Status

**LOCAL TESTING**: ✅ COMPLETE
- Package builds successfully
- Tarball created: `necro-bridge-core-1.0.0.tgz`
- Local installation verified
- CLI commands functional

**NPM PUBLICATION**: ⏳ READY FOR MANUAL EXECUTION

To publish, run:
```bash
cd toolkit
npm login
npm publish --access public
```

---

**Note**: This is an optional task for the hackathon demo. The package is fully prepared and tested for publication, but actual NPM publication requires manual authentication and is at the discretion of the project maintainer.
