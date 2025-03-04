import { createServer, IncomingMessage } from 'node:http'
import * as fs from 'node:fs'
import * as path from 'node:path'

// Function to read request body as a string
async function getRequestBody(req: IncomingMessage): Promise<string> {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', (chunk) => {
      body += chunk.toString();
    });
    req.on('end', () => {
      resolve(body);
    });
    req.on('error', (err) => {
      reject(err);
    });
  });
}

// Create a simple HTTP server
const server = createServer(async (req, res) => {
  if (req.method === 'POST' && req.url === "/tlsp/test") {
    try {
      const body = await getRequestBody(req);
      console.log('Request Body:', body);
      res.writeHead(200, { 'Content-Type': 'text/plain' });
      res.end('Body received: ' + body);
    } catch (err) {
      res.writeHead(500, { 'Content-Type': 'text/plain' });
      res.end('Error reading request body');
    }
  } else if (req.method === 'GET') {
    console.log('Request URL:', req.url);
    const filepath = path.join('dist', req.url || '');
    if (!fs.existsSync(filepath)) {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('404 Not Found');
      return;
    }
    const extname = path.extname(filepath);
    const content = fs.readFileSync(filepath);
    if (extname === '.js') {
      res.writeHead(200, { 'Content-Type': 'text/javascript' });
    } else if (extname === '.html') {
      res.writeHead(200, { 'Content-Type': 'text/html' });
    } else if (extname === '.css') {
      res.writeHead(200, { 'Content-Type': 'text/css' });
    } else if (extname === '.json') {
      res.writeHead(200, { 'Content-Type': 'application/json' });
    } else if (extname === '.ico') {
      res.writeHead(200, { 'Content-Type': 'image/x-icon' });
    }
    res.end(content);
  }
});

server.listen(3000, () => {
  console.log('Server running on port 3000');
});