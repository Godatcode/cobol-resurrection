# 🚀 Deployment Guide

This guide covers deploying your legacy bridge server to production.

## Prerequisites

- Working bridge server (tested locally)
- Git repository
- Account on deployment platform (Heroku, Railway, Fly.io, etc.)

## Platform-Specific Guides

### 🚂 Railway (Recommended - Easiest)

**Why Railway?**
- Free tier available
- Automatic deployments from Git
- Easy environment variable management
- Good for hobby projects

**Steps:**

1. **Create `railway.json`:**
```json
{
  "$schema": "https://railway.app/railway.schema.json",
  "build": {
    "builder": "NIXPACKS"
  },
  "deploy": {
    "startCommand": "node universal-server.js",
    "restartPolicyType": "ON_FAILURE",
    "restartPolicyMaxRetries": 10
  }
}
```

2. **Push to GitHub:**
```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/yourusername/your-repo.git
git push -u origin main
```

3. **Deploy on Railway:**
- Go to [railway.app](https://railway.app)
- Click "New Project" → "Deploy from GitHub repo"
- Select your repository
- Railway will auto-detect Node.js and deploy

4. **Set Environment Variables:**
- In Railway dashboard, go to Variables
- Add: `PORT=3001` (or Railway will assign one)
- Add any other env vars from your `.env`

5. **Get Your URL:**
- Railway provides a URL like: `your-app.railway.app`
- Test: `curl https://your-app.railway.app/api/health`

### 🟣 Heroku

**Steps:**

1. **Create `Procfile`:**
```
web: node universal-server.js
```

2. **Create `heroku.yml` (if using Docker):**
```yaml
build:
  docker:
    web: Dockerfile
run:
  web: node universal-server.js
```

3. **Deploy:**
```bash
heroku login
heroku create your-app-name
git push heroku main
```

4. **Set Environment Variables:**
```bash
heroku config:set PORT=3001
heroku config:set NODE_ENV=production
```

5. **View Logs:**
```bash
heroku logs --tail
```

### 🪰 Fly.io

**Steps:**

1. **Install Fly CLI:**
```bash
curl -L https://fly.io/install.sh | sh
```

2. **Login:**
```bash
fly auth login
```

3. **Create `fly.toml`:**
```toml
app = "your-app-name"

[build]
  builder = "heroku/buildpacks:20"

[env]
  PORT = "8080"

[[services]]
  internal_port = 8080
  protocol = "tcp"

  [[services.ports]]
    handlers = ["http"]
    port = 80

  [[services.ports]]
    handlers = ["tls", "http"]
    port = 443
```

4. **Deploy:**
```bash
fly launch
fly deploy
```

5. **Set Secrets:**
```bash
fly secrets set API_KEY=your-secret-key
```

### 🐳 Docker + Any Platform

**Create `Dockerfile`:**
```dockerfile
FROM node:18-alpine

# Install legacy compilers (if needed)
RUN apk add --no-cache \
    gnu-cobol \
    gfortran

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install --production

# Copy application files
COPY . .

# Expose port
EXPOSE 3001

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3001/api/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Start server
CMD ["node", "universal-server.js"]
```

**Create `docker-compose.yml`:**
```yaml
version: '3.8'

services:
  bridge-server:
    build: .
    ports:
      - "3001:3001"
    environment:
      - NODE_ENV=production
      - PORT=3001
    restart: unless-stopped
    volumes:
      - ./logs:/app/logs
```

**Build and Run:**
```bash
docker build -t legacy-bridge .
docker run -p 3001:3001 legacy-bridge
```

### ☁️ AWS EC2

**Steps:**

1. **Launch EC2 Instance:**
- Choose Ubuntu 22.04 LTS
- t2.micro (free tier eligible)
- Configure security group: Allow port 3001

2. **SSH into Instance:**
```bash
ssh -i your-key.pem ubuntu@your-ec2-ip
```

3. **Install Dependencies:**
```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install Node.js
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install -y nodejs

# Install legacy compilers
sudo apt install -y gnucobol gfortran fpc freebasic

# Install PM2 (process manager)
sudo npm install -g pm2
```

4. **Deploy Application:**
```bash
# Clone your repo
git clone https://github.com/yourusername/your-repo.git
cd your-repo

# Install dependencies
npm install --production

# Start with PM2
pm2 start universal-server.js --name legacy-bridge
pm2 save
pm2 startup
```

5. **Configure Nginx (Optional):**
```bash
sudo apt install -y nginx

# Create Nginx config
sudo nano /etc/nginx/sites-available/legacy-bridge
```

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:3001;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}
```

```bash
# Enable site
sudo ln -s /etc/nginx/sites-available/legacy-bridge /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

## Environment Variables

Create a `.env` file (or set in platform dashboard):

```bash
# Server
PORT=3001
NODE_ENV=production

# Security
API_KEY=your-secret-key
ALLOWED_ORIGINS=https://your-frontend.com

# Timeouts
EXECUTION_TIMEOUT=5000

# Logging
LOG_LEVEL=info
```

## Security Checklist

### ✅ Essential Security Measures

1. **Use HTTPS:**
   - Most platforms provide this automatically
   - For custom domains, use Let's Encrypt

2. **Add Rate Limiting:**
```javascript
const rateLimit = require('express-rate-limit');

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100 // limit each IP to 100 requests per windowMs
});

app.use('/api/', limiter);
```

3. **Add API Key Authentication:**
```javascript
const authenticateApiKey = (req, res, next) => {
  const apiKey = req.headers['x-api-key'];
  
  if (!apiKey || apiKey !== process.env.API_KEY) {
    return res.status(401).json({ error: 'UNAUTHORIZED' });
  }
  
  next();
};

app.use('/api/calculate', authenticateApiKey);
```

4. **Validate Input:**
```javascript
const { body, validationResult } = require('express-validator');

app.post('/api/calculate/:language',
  body('param1').isNumeric().isFloat({ min: 0 }),
  body('param2').isNumeric().isFloat({ min: 0 }),
  (req, res, next) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }
    next();
  }
);
```

5. **Set Security Headers:**
```javascript
const helmet = require('helmet');
app.use(helmet());
```

6. **Configure CORS Properly:**
```javascript
const cors = require('cors');

app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(',') || '*',
  methods: ['GET', 'POST'],
  allowedHeaders: ['Content-Type', 'x-api-key']
}));
```

## Monitoring and Logging

### Add Logging

```javascript
const winston = require('winston');

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' })
  ]
});

if (process.env.NODE_ENV !== 'production') {
  logger.add(new winston.transports.Console({
    format: winston.format.simple()
  }));
}
```

### Add Health Checks

```javascript
app.get('/api/health', (req, res) => {
  res.json({
    status: 'OPERATIONAL',
    uptime: process.uptime(),
    timestamp: new Date().toISOString(),
    memory: process.memoryUsage(),
    supported_languages: getSupportedLanguages()
  });
});
```

### Monitor with PM2

```bash
# View logs
pm2 logs legacy-bridge

# Monitor resources
pm2 monit

# View status
pm2 status
```

## Performance Optimization

### 1. Enable Compression

```javascript
const compression = require('compression');
app.use(compression());
```

### 2. Add Caching

```javascript
const NodeCache = require('node-cache');
const cache = new NodeCache({ stdTTL: 600 }); // 10 minutes

app.post('/api/calculate/:language', async (req, res) => {
  const cacheKey = `${req.params.language}:${JSON.stringify(req.body)}`;
  
  // Check cache
  const cached = cache.get(cacheKey);
  if (cached) {
    return res.json({ ...cached, cached: true });
  }
  
  // Execute and cache
  const result = await bridge.execute(req.body);
  cache.set(cacheKey, result);
  
  res.json(result);
});
```

### 3. Use Clustering

```javascript
const cluster = require('cluster');
const os = require('os');

if (cluster.isMaster) {
  const numCPUs = os.cpus().length;
  
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }
  
  cluster.on('exit', (worker) => {
    console.log(`Worker ${worker.process.pid} died`);
    cluster.fork();
  });
} else {
  // Start server
  app.listen(PORT);
}
```

## Troubleshooting

### Binary Not Found in Production

**Problem:** Binary works locally but not in production

**Solution:**
1. Ensure binary is committed to git (not in .gitignore)
2. Check file permissions: `chmod +x legacy/cobol/mortgage`
3. Use absolute paths in `getBinaryPath()`

### Timeout Errors

**Problem:** Calculations timeout in production

**Solution:**
1. Increase timeout: `timeout: 10000`
2. Check if binary is slower in production environment
3. Monitor CPU/memory usage

### Port Already in Use

**Problem:** `Error: listen EADDRINUSE`

**Solution:**
```javascript
const PORT = process.env.PORT || 3001;
```

Most platforms assign a port via `process.env.PORT`

## Backup and Recovery

### Database Backups (if using one)

```bash
# Automated daily backups
0 2 * * * pg_dump mydb > /backups/mydb_$(date +\%Y\%m\%d).sql
```

### Application Backups

```bash
# Backup entire application
tar -czf backup_$(date +%Y%m%d).tar.gz /path/to/app
```

## Scaling

### Horizontal Scaling

- Use load balancer (Nginx, AWS ALB)
- Deploy multiple instances
- Use Redis for shared caching

### Vertical Scaling

- Upgrade server resources (CPU, RAM)
- Optimize binary execution
- Use faster storage (SSD)

## Cost Optimization

### Free Tier Options

- **Railway:** 500 hours/month free
- **Heroku:** 550 hours/month free (with credit card)
- **Fly.io:** 3 shared-cpu-1x VMs free

### Reduce Costs

1. Use caching to reduce binary executions
2. Implement rate limiting
3. Use serverless for low traffic
4. Monitor and optimize resource usage

## Next Steps

1. ✅ Choose a deployment platform
2. ✅ Set up CI/CD pipeline
3. ✅ Configure monitoring and alerts
4. ✅ Set up automated backups
5. ✅ Document your deployment process
6. ✅ Test disaster recovery

Happy deploying! 🚀
