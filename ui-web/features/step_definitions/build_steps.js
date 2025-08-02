const { Given, When, Then } = require('cucumber');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const { expect } = require('chai');
const { promisify } = require('util');

const execAsync = promisify(exec);

Given('I am in the ui-web directory', function () {
  this.workingDirectory = process.cwd();
});

When('I run the build command with NODE_OPTIONS set to --openssl-legacy-provider', { timeout: 180000 }, async function () {
  try {
    const { stdout, stderr } = await execAsync('NODE_OPTIONS="--openssl-legacy-provider" npm run build', {
      cwd: this.workingDirectory
    });
    this.buildOutput = stdout;
    this.buildError = stderr;
    this.buildSucceeded = true;
  } catch (error) {
    this.buildOutput = error.stdout;
    this.buildError = error.stderr;
    this.buildSucceeded = false;
    this.buildExitCode = error.code;
  }
});

Then('the build should complete successfully', function () {
  expect(this.buildSucceeded).to.be.true;
  expect(this.buildExitCode).to.be.undefined;
});

Then('the build output directory should exist', function () {
  const buildPath = path.join(this.workingDirectory, 'build');
  expect(fs.existsSync(buildPath)).to.be.true;
});

Then('the build should contain the necessary production files', function () {
  const buildPath = path.join(this.workingDirectory, 'build');
  const indexPath = path.join(buildPath, 'index.html');
  const staticPath = path.join(buildPath, 'static');
  
  expect(fs.existsSync(indexPath)).to.be.true;
  expect(fs.existsSync(staticPath)).to.be.true;
});

When('I check the build script in package.json', function () {
  const packageJsonPath = path.join(this.workingDirectory, 'package.json');
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
  this.buildScript = packageJson.scripts.build;
});

Then('it should include NODE_OPTIONS=--openssl-legacy-provider', function () {
  expect(this.buildScript).to.include('NODE_OPTIONS=--openssl-legacy-provider');
});

Then('it should use craco build command', function () {
  expect(this.buildScript).to.include('craco build');
});

Given('the application has been built successfully', { timeout: 180000 }, async function () {
  // Run build if not already done
  if (!this.buildSucceeded) {
    await execAsync('NODE_OPTIONS="--openssl-legacy-provider" npm run build', {
      cwd: this.workingDirectory
    });
  }
});

When('I check the build directory', function () {
  this.buildPath = path.join(this.workingDirectory, 'build');
  this.buildContents = fs.readdirSync(this.buildPath);
});

Then('it should contain an index.html file', function () {
  const indexPath = path.join(this.buildPath, 'index.html');
  expect(fs.existsSync(indexPath)).to.be.true;
});

Then('it should contain static assets in the correct folders', function () {
  const staticPath = path.join(this.buildPath, 'static');
  expect(fs.existsSync(staticPath)).to.be.true;
  
  // Check for standard CRA build structure
  const jsPath = path.join(staticPath, 'js');
  const cssPath = path.join(staticPath, 'css');
  
  expect(fs.existsSync(jsPath)).to.be.true;
  expect(fs.existsSync(cssPath)).to.be.true;
});

Then('all JavaScript files should be minified', function () {
  const jsPath = path.join(this.buildPath, 'static', 'js');
  const jsFiles = fs.readdirSync(jsPath).filter(file => file.endsWith('.js'));
  
  jsFiles.forEach(file => {
    const filePath = path.join(jsPath, file);
    const content = fs.readFileSync(filePath, 'utf8');
    // Basic check for minification - no excessive whitespace or newlines
    const lines = content.split('\n');
    expect(lines.length).to.be.lessThan(100); // Minified files typically have few lines
  });
});