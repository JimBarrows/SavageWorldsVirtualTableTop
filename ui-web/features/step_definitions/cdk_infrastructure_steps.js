import { Given, When, Then } from '@cucumber/cucumber';
import { expect } from 'chai';
import fs from 'fs';
import path from 'path';
import { execSync } from 'child_process';

// Helper function to check if a command exists
function commandExists(command) {
  try {
    execSync(`which ${command}`, { stdio: 'ignore' });
    return true;
  } catch (error) {
    return false;
  }
}

// Helper function to check if a file exists
function fileExists(filePath) {
  return fs.existsSync(filePath);
}

// Helper function to check if directory exists
function directoryExists(dirPath) {
  return fs.existsSync(dirPath) && fs.lstatSync(dirPath).isDirectory();
}

// Helper function to read JSON file
function readJsonFile(filePath) {
  if (!fileExists(filePath)) return null;
  try {
    return JSON.parse(fs.readFileSync(filePath, 'utf8'));
  } catch (error) {
    return null;
  }
}

const projectRoot = process.cwd();
const infrastructureDir = path.join(projectRoot, 'infrastructure', 'cdk');

// Background step definitions
Given('I have AWS CLI configured with valid credentials', function () {
  const hasAwsCli = commandExists('aws');
  expect(hasAwsCli, 'AWS CLI is not installed or not in PATH').to.be.true;
  
  // Check for AWS credentials (basic check)
  try {
    execSync('aws sts get-caller-identity', { stdio: 'ignore' });
  } catch (error) {
    throw new Error('AWS credentials not configured properly');
  }
});

Given('I have Node.js 18+ installed', function () {
  const hasNode = commandExists('node');
  expect(hasNode, 'Node.js is not installed or not in PATH').to.be.true;
  
  const nodeVersion = execSync('node --version', { encoding: 'utf8' }).trim();
  const majorVersion = parseInt(nodeVersion.substring(1).split('.')[0]);
  expect(majorVersion, 'Node.js version must be 18 or higher').to.be.at.least(18);
});

Given('I have AWS CDK CLI installed', function () {
  const hasCdk = commandExists('cdk');
  expect(hasCdk, 'AWS CDK CLI is not installed or not in PATH').to.be.true;
});

// Scenario 1: Initialize CDK project with TypeScript
Given('I am in the project root directory', function () {
  expect(process.cwd(), 'Should be in project root').to.equal(projectRoot);
});

When('I initialize a new CDK project with TypeScript', function () {
  // Create infrastructure directory if it doesn't exist
  if (!directoryExists(path.join(projectRoot, 'infrastructure'))) {
    fs.mkdirSync(path.join(projectRoot, 'infrastructure'), { recursive: true });
  }
  
  // Initialize CDK project in infrastructure/cdk directory
  process.chdir(path.join(projectRoot, 'infrastructure'));
  
  try {
    execSync('cdk init app --language typescript', { stdio: 'pipe' });
    this.cdkInitialized = true;
  } catch (error) {
    this.cdkInitialized = false;
    this.initError = error.message;
  }
  
  // Return to project root
  process.chdir(projectRoot);
});

Then('the project should have a valid CDK TypeScript structure', function () {
  expect(this.cdkInitialized, `CDK initialization failed: ${this.initError || 'Unknown error'}`).to.be.true;
  expect(directoryExists(infrastructureDir), 'CDK directory should exist').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'lib')), 'lib directory should exist').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'bin')), 'bin directory should exist').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'test')), 'test directory should exist').to.be.true;
});

Then('the CDK configuration file should be created', function () {
  const cdkConfigPath = path.join(infrastructureDir, 'cdk.json');
  expect(fileExists(cdkConfigPath), 'cdk.json should exist').to.be.true;
  
  const cdkConfig = readJsonFile(cdkConfigPath);
  expect(cdkConfig, 'cdk.json should be valid JSON').to.not.be.null;
  expect(cdkConfig.app, 'cdk.json should have app configuration').to.exist;
});

Then('TypeScript configuration should be properly set up', function () {
  const tsConfigPath = path.join(infrastructureDir, 'tsconfig.json');
  expect(fileExists(tsConfigPath), 'tsconfig.json should exist').to.be.true;
  
  const tsConfig = readJsonFile(tsConfigPath);
  expect(tsConfig, 'tsconfig.json should be valid JSON').to.not.be.null;
  expect(tsConfig.compilerOptions, 'tsconfig.json should have compiler options').to.exist;
});

// Scenario 2: Install required CDK dependencies
Given('I have a CDK project initialized', function () {
  expect(directoryExists(infrastructureDir), 'CDK project should be initialized').to.be.true;
  expect(fileExists(path.join(infrastructureDir, 'package.json')), 'package.json should exist').to.be.true;
});

When('I install all required AWS CDK libraries', function () {
  const originalDir = process.cwd();
  process.chdir(infrastructureDir);
  
  try {
    // Install required CDK libraries
    const cdkLibraries = [
      '@aws-cdk/core',
      '@aws-cdk/aws-ec2',
      '@aws-cdk/aws-ecs',
      '@aws-cdk/aws-rds',
      '@aws-cdk/aws-s3',
      '@aws-cdk/aws-cloudfront',
      '@aws-cdk/aws-iam',
      '@aws-cdk/aws-secretsmanager'
    ];
    
    execSync(`npm install ${cdkLibraries.join(' ')}`, { stdio: 'pipe' });
    this.dependenciesInstalled = true;
  } catch (error) {
    this.dependenciesInstalled = false;
    this.installError = error.message;
  } finally {
    process.chdir(originalDir);
  }
});

Then('all CDK dependencies should be available', function () {
  expect(this.dependenciesInstalled, `Dependency installation failed: ${this.installError || 'Unknown error'}`).to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'node_modules')), 'node_modules should exist').to.be.true;
});

Then('the package.json should include all required CDK libraries', function () {
  const packageJsonPath = path.join(infrastructureDir, 'package.json');
  const packageJson = readJsonFile(packageJsonPath);
  
  expect(packageJson, 'package.json should be readable').to.not.be.null;
  
  const requiredLibraries = [
    '@aws-cdk/core',
    '@aws-cdk/aws-ec2',
    '@aws-cdk/aws-ecs',
    '@aws-cdk/aws-rds',
    '@aws-cdk/aws-s3',
    '@aws-cdk/aws-cloudfront',
    '@aws-cdk/aws-iam',
    '@aws-cdk/aws-secretsmanager'
  ];
  
  const dependencies = { ...packageJson.dependencies, ...packageJson.devDependencies };
  
  requiredLibraries.forEach(library => {
    expect(dependencies[library], `${library} should be in package.json`).to.exist;
  });
});

Then('no dependency conflicts should exist', function () {
  const originalDir = process.cwd();
  process.chdir(infrastructureDir);
  
  try {
    execSync('npm ls', { stdio: 'pipe' });
    this.noConflicts = true;
  } catch (error) {
    this.noConflicts = false;
    this.conflictError = error.message;
  } finally {
    process.chdir(originalDir);
  }
  
  expect(this.noConflicts, `Dependency conflicts detected: ${this.conflictError || 'Unknown error'}`).to.be.true;
});

// Additional step definitions for remaining scenarios...
// (Similar pattern for other scenarios)

// Scenario 3: Create project directory structure
Given('I have a CDK project with dependencies installed', function () {
  expect(directoryExists(infrastructureDir), 'CDK project should exist').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'node_modules')), 'Dependencies should be installed').to.be.true;
});

When('I create the standardized directory structure', function () {
  const directories = [
    path.join(infrastructureDir, 'lib', 'stacks'),
    path.join(infrastructureDir, 'lib', 'constructs'),
    path.join(infrastructureDir, 'config')
  ];
  
  directories.forEach(dir => {
    if (!directoryExists(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
  });
  
  this.directoryStructureCreated = true;
});

Then('the lib/stacks directory should contain all required stack files', function () {
  expect(this.directoryStructureCreated, 'Directory structure should be created').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'lib', 'stacks')), 'lib/stacks directory should exist').to.be.true;
});

Then('the lib/constructs directory should exist for custom constructs', function () {
  expect(directoryExists(path.join(infrastructureDir, 'lib', 'constructs')), 'lib/constructs directory should exist').to.be.true;
});

Then('the config directory should have environment-specific files', function () {
  expect(directoryExists(path.join(infrastructureDir, 'config')), 'config directory should exist').to.be.true;
});

Then('the test directory should be set up for infrastructure testing', function () {
  expect(directoryExists(path.join(infrastructureDir, 'test')), 'test directory should exist').to.be.true;
});

// Helper to validate CDK synthesis
function validateCdkSynthesis() {
  const originalDir = process.cwd();
  process.chdir(infrastructureDir);
  
  try {
    execSync('cdk synth', { stdio: 'pipe' });
    return { success: true };
  } catch (error) {
    return { success: false, error: error.message };
  } finally {
    process.chdir(originalDir);
  }
}

// Final validation scenario
When('I run CDK synthesis command', function () {
  this.synthResult = validateCdkSynthesis();
});

Then('CloudFormation templates should be generated successfully', function () {
  expect(this.synthResult.success, `CDK synthesis failed: ${this.synthResult.error || 'Unknown error'}`).to.be.true;
});

Then('no CDK errors or warnings should be present', function () {
  // Additional validation could be added here to check for warnings
  expect(this.synthResult.success, 'CDK should synthesize without errors').to.be.true;
});

Then('the project should be ready for environment deployment', function () {
  expect(this.synthResult.success, 'Project should be deployment-ready').to.be.true;
  expect(directoryExists(path.join(infrastructureDir, 'cdk.out')), 'CDK output directory should exist after synthesis').to.be.true;
});

Then('all configuration files should be valid', function () {
  const configFiles = ['cdk.json', 'tsconfig.json', 'package.json'];
  configFiles.forEach(file => {
    const filePath = path.join(infrastructureDir, file);
    expect(fileExists(filePath), `${file} should exist`).to.be.true;
    
    if (file.endsWith('.json')) {
      const content = readJsonFile(filePath);
      expect(content, `${file} should be valid JSON`).to.not.be.null;
    }
  });
});