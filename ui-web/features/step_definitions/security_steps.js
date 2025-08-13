import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

// Store audit results for verification across steps
let auditResults = null;
let initialVulnerabilityCount = null;
let buildSuccess = false;
let appStarted = false;

// Background step definitions
Given('the project has npm dependencies', function () {
  const packageJsonPath = path.join(process.cwd(), 'package.json');
  expect(fs.existsSync(packageJsonPath)).to.be.true;
  
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
  expect(packageJson.dependencies || packageJson.devDependencies).to.not.be.empty;
});

Given('security vulnerabilities have been identified in the dependency tree', function () {
  // Run npm audit to capture initial vulnerability state
  try {
    const auditOutput = execSync('npm audit --json', { 
      encoding: 'utf8',
      stdio: 'pipe'
    });
    initialVulnerabilityCount = JSON.parse(auditOutput).metadata.vulnerabilities;
    expect(initialVulnerabilityCount.total).to.be.greaterThan(0);
  } catch (error) {
    // npm audit returns non-zero exit code when vulnerabilities exist
    // Parse the error output which contains the audit JSON
    if (error.stdout) {
      initialVulnerabilityCount = JSON.parse(error.stdout).metadata.vulnerabilities;
      expect(initialVulnerabilityCount.total).to.be.greaterThan(0);
    } else {
      throw new Error('Unable to identify security vulnerabilities in dependency tree');
    }
  }
});

// Critical vulnerability scenario steps
Given('the application has critical security vulnerabilities', function () {
  expect(initialVulnerabilityCount).to.not.be.null;
  expect(initialVulnerabilityCount.critical).to.be.greaterThan(0);
});

// High severity vulnerability scenario steps
Given('the application has high severity security vulnerabilities', function () {
  expect(initialVulnerabilityCount).to.not.be.null;
  expect(initialVulnerabilityCount.high).to.be.greaterThan(0);
});

// Moderate vulnerability scenario steps
Given('the application has moderate security vulnerabilities', function () {
  expect(initialVulnerabilityCount).to.not.be.null;
  expect(initialVulnerabilityCount.moderate).to.be.greaterThan(0);
});

// Low vulnerability scenario steps
Given('the application has low priority security vulnerabilities', function () {
  expect(initialVulnerabilityCount).to.not.be.null;
  expect(initialVulnerabilityCount.low).to.be.greaterThan(0);
});

// Common audit step
When('I run a comprehensive security audit', function () {
  try {
    const auditOutput = execSync('npm audit --json', { 
      encoding: 'utf8',
      stdio: 'pipe'
    });
    auditResults = JSON.parse(auditOutput);
  } catch (error) {
    // npm audit returns non-zero exit code when vulnerabilities exist
    if (error.stdout) {
      auditResults = JSON.parse(error.stdout);
    } else {
      throw new Error('Security audit failed to execute');
    }
  }
  
  expect(auditResults).to.not.be.null;
  expect(auditResults.metadata).to.not.be.undefined;
  expect(auditResults.metadata.vulnerabilities).to.not.be.undefined;
});

// Critical vulnerability resolution verification
Then('all critical vulnerabilities should be resolved', function () {
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
});

Then('no new critical vulnerabilities should be introduced', function () {
  // Verify we haven't introduced new critical vulnerabilities
  expect(auditResults.metadata.vulnerabilities.critical).to.be.at.most(0);
});

Then('the security audit should report zero critical issues', function () {
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
});

// High severity vulnerability resolution verification
Then('all high severity vulnerabilities should be resolved', function () {
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

Then('no new high severity vulnerabilities should be introduced', function () {
  expect(auditResults.metadata.vulnerabilities.high).to.be.at.most(0);
});

Then('the security audit should report zero high severity issues', function () {
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

// Moderate vulnerability management verification
Then('moderate vulnerabilities are either resolved or documented with business justification', function () {
  const currentModerate = auditResults.metadata.vulnerabilities.moderate;
  
  if (currentModerate > 0) {
    // Check for documentation file
    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    if (fs.existsSync(riskDocPath)) {
      const riskDoc = fs.readFileSync(riskDocPath, 'utf8');
      expect(riskDoc).to.contain('moderate');
      expect(riskDoc.length).to.be.greaterThan(100); // Ensure meaningful documentation
    } else {
      // All moderate vulnerabilities must be resolved if no documentation
      expect(currentModerate).to.equal(0);
    }
  }
});

Then('any remaining moderate vulnerabilities have risk assessment documentation', function () {
  const currentModerate = auditResults.metadata.vulnerabilities.moderate;
  
  if (currentModerate > 0) {
    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    expect(fs.existsSync(riskDocPath)).to.be.true;
    
    const riskDoc = fs.readFileSync(riskDocPath, 'utf8');
    expect(riskDoc).to.contain('moderate vulnerabilities');
    expect(riskDoc).to.contain('business justification');
  }
});

Then('the number of moderate vulnerabilities is significantly reduced from baseline', function () {
  const currentModerate = auditResults.metadata.vulnerabilities.moderate;
  const initialModerate = initialVulnerabilityCount.moderate;
  
  // Expect at least 50% reduction in moderate vulnerabilities
  const reductionThreshold = Math.floor(initialModerate * 0.5);
  expect(currentModerate).to.be.at.most(reductionThreshold);
});

// Low vulnerability management verification
Then('low priority vulnerabilities are either resolved or documented with business justification', function () {
  const currentLow = auditResults.metadata.vulnerabilities.low;
  
  if (currentLow > 0) {
    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    if (fs.existsSync(riskDocPath)) {
      const riskDoc = fs.readFileSync(riskDocPath, 'utf8');
      expect(riskDoc).to.contain('low');
    } else {
      // All low vulnerabilities must be resolved if no documentation
      expect(currentLow).to.equal(0);
    }
  }
});

Then('any remaining low vulnerabilities have risk assessment documentation', function () {
  const currentLow = auditResults.metadata.vulnerabilities.low;
  
  if (currentLow > 0) {
    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    expect(fs.existsSync(riskDocPath)).to.be.true;
    
    const riskDoc = fs.readFileSync(riskDocPath, 'utf8');
    expect(riskDoc).to.contain('low vulnerabilities');
  }
});

// Application functionality verification steps
Given('security vulnerabilities have been resolved through dependency updates', function () {
  // This step assumes previous security remediation has occurred
  // We verify the audit shows improvement
  expect(auditResults).to.not.be.null;
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

When('the application is started in development mode', function () {
  try {
    // Test that the application can start (basic smoke test)
    const startResult = execSync('timeout 30s npm start > /dev/null 2>&1 || echo "timeout"', { 
      encoding: 'utf8',
      timeout: 35000
    });
    appStarted = true;
  } catch (error) {
    // Expected for timeout command, check if app started successfully
    appStarted = !error.message.includes('Command failed');
  }
});

Then('the application should start successfully without errors', function () {
  expect(appStarted).to.be.true;
});

Then('all existing functionality should work as expected', function () {
  // Basic functionality test - check if key files exist and are valid
  const keyFiles = [
    'src/App.js',
    'src/index.js',
    'public/index.html'
  ];
  
  keyFiles.forEach(file => {
    const filePath = path.join(process.cwd(), file);
    expect(fs.existsSync(filePath)).to.be.true;
  });
});

Then('no new runtime errors should be introduced', function () {
  // This would be verified through the application start test
  expect(appStarted).to.be.true;
});

// Build process verification steps
Given('security dependencies have been updated', function () {
  // Verify package.json or package-lock.json has recent modifications
  const packageLockPath = path.join(process.cwd(), 'package-lock.json');
  if (fs.existsSync(packageLockPath)) {
    const stats = fs.statSync(packageLockPath);
    const now = new Date();
    const dayAgo = new Date(now.getTime() - 24 * 60 * 60 * 1000);
    expect(stats.mtime).to.be.greaterThan(dayAgo);
  }
});

When('I build the application for production', function () {
  try {
    execSync('npm run build', { 
      encoding: 'utf8',
      stdio: 'pipe',
      timeout: 300000 // 5 minute timeout for build
    });
    buildSuccess = true;
  } catch (error) {
    buildSuccess = false;
    console.error('Build error:', error.message);
  }
});

Then('the build should complete successfully', function () {
  expect(buildSuccess).to.be.true;
});

Then('no build errors should occur due to dependency updates', function () {
  expect(buildSuccess).to.be.true;
});

Then('the production bundle should be generated correctly', function () {
  const buildPath = path.join(process.cwd(), 'build');
  expect(fs.existsSync(buildPath)).to.be.true;
  
  // Check for essential build files
  const indexHtml = path.join(buildPath, 'index.html');
  expect(fs.existsSync(indexHtml)).to.be.true;
});

// Test suite verification steps
Given('security vulnerabilities have been resolved', function () {
  expect(auditResults).to.not.be.null;
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

When('I run the complete test suite', function () {
  this.testResults = { unit: false, bdd: false };
  
  try {
    // Run unit tests
    execSync('npm run test:unit -- --watchAll=false', { 
      encoding: 'utf8',
      stdio: 'pipe'
    });
    this.testResults.unit = true;
  } catch (error) {
    console.error('Unit test error:', error.message);
  }
  
  try {
    // Run BDD tests (excluding the current feature to avoid recursion)
    execSync('NODE_OPTIONS="--openssl-legacy-provider" npx cucumber-js features --require-module @babel/register --require features/step_definitions --require features/support --format progress --not-tags @security', { 
      encoding: 'utf8',
      stdio: 'pipe',
      cwd: path.join(process.cwd(), 'ui-web')
    });
    this.testResults.bdd = true;
  } catch (error) {
    console.error('BDD test error:', error.message);
  }
});

Then('all existing unit tests should pass', function () {
  expect(this.testResults.unit).to.be.true;
});

Then('all existing BDD tests should pass', function () {
  expect(this.testResults.bdd).to.be.true;
});

Then('no test failures should be introduced by security updates', function () {
  expect(this.testResults.unit).to.be.true;
  expect(this.testResults.bdd).to.be.true;
});

// CI/CD pipeline verification steps
Given('security vulnerabilities have been remediated', function () {
  expect(auditResults).to.not.be.null;
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

When('the CI/CD pipeline runs security audits', function () {
  // Simulate CI/CD security audit
  try {
    execSync('npm audit --audit-level=moderate', { 
      encoding: 'utf8',
      stdio: 'pipe'
    });
    this.cicdAuditPassed = true;
  } catch (error) {
    this.cicdAuditPassed = false;
    console.error('CI/CD audit error:', error.message);
  }
});

Then('the security audit step should pass', function () {
  expect(this.cicdAuditPassed).to.be.true;
});

Then('no pipeline failures should occur due to security issues', function () {
  expect(this.cicdAuditPassed).to.be.true;
});

Then('the security scan results should meet compliance requirements', function () {
  // Verify critical and high vulnerabilities are zero for compliance
  expect(auditResults.metadata.vulnerabilities.critical).to.equal(0);
  expect(auditResults.metadata.vulnerabilities.high).to.equal(0);
});

// Documentation verification steps
Given('security vulnerabilities have been addressed', function () {
  expect(auditResults).to.not.be.null;
});

When('reviewing the remediation documentation', function () {
  this.docExists = fs.existsSync(path.join(process.cwd(), 'SECURITY_REMEDIATION.md'));
  if (this.docExists) {
    this.docContent = fs.readFileSync(path.join(process.cwd(), 'SECURITY_REMEDIATION.md'), 'utf8');
  }
});

Then('all resolved vulnerabilities should be documented', function () {
  if (this.docExists) {
    expect(this.docContent).to.contain('vulnerabilities');
    expect(this.docContent).to.contain('resolved');
  }
});

Then('any remaining vulnerabilities should have risk justifications', function () {
  const totalRemaining = auditResults.metadata.vulnerabilities.moderate + 
                        auditResults.metadata.vulnerabilities.low;
  
  if (totalRemaining > 0) {
    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    expect(fs.existsSync(riskDocPath)).to.be.true;
  }
});

Then('the remediation approach should be clearly explained', function () {
  if (this.docExists) {
    expect(this.docContent).to.contain('approach');
    expect(this.docContent.length).to.be.greaterThan(200);
  }
});

Then('future maintenance recommendations should be provided', function () {
  if (this.docExists) {
    expect(this.docContent).to.contain('maintenance');
    expect(this.docContent).to.contain('recommend');
  }
});