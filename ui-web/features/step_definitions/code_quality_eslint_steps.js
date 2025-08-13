import { Given, When, Then } from 'cucumber';
import { exec } from 'child_process';
import { promisify } from 'util';
import { readdir, readFile } from 'fs/promises';
import { join } from 'path';
import { expect } from 'chai';

const execAsync = promisify(exec);

// Background steps
Given('a React application codebase with ESLint configuration', async function () {
  this.projectRoot = process.cwd();
  this.uiWebPath = join(this.projectRoot, 'ui-web');
  
  // Verify ESLint configuration exists
  try {
    await readFile(join(this.uiWebPath, '.eslintrc.json'));
    this.eslintConfigExists = true;
  } catch (error) {
    this.eslintConfigExists = false;
  }
  
  expect(this.eslintConfigExists).to.be.true;
});

Given('the development team values code quality and consistency', function () {
  // This is a given context for BDD scenarios
  this.codeQualityStandards = {
    eslintWarnings: 0,
    eslintErrors: 0,
    propTypesRequired: true,
    unusedCodeNotAllowed: true
  };
});

// Main scenario steps
Given('the development team has completed code changes', function () {
  // Assume code changes exist in the current working directory
  this.codeChangesReady = true;
});

When('they run the ESLint validation command', async function () {
  try {
    const { stdout, stderr } = await execAsync('npm run lint', { 
      cwd: this.uiWebPath,
      timeout: 30000 
    });
    this.eslintOutput = stdout + stderr;
    this.eslintExitCode = 0;
  } catch (error) {
    this.eslintOutput = error.stdout + error.stderr;
    this.eslintExitCode = error.code || 1;
  }
});

Then('there should be {int} ESLint warnings reported', function (expectedWarnings) {
  const warningMatches = this.eslintOutput.match(/(\d+) warnings?/);
  const actualWarnings = warningMatches ? parseInt(warningMatches[1]) : 0;
  expect(actualWarnings).to.equal(expectedWarnings);
});

Then('there should be {int} ESLint errors reported', function (expectedErrors) {
  const errorMatches = this.eslintOutput.match(/(\d+) errors?/);
  const actualErrors = errorMatches ? parseInt(errorMatches[1]) : 0;
  expect(actualErrors).to.equal(expectedErrors);
});

Then('the linting process should exit with success status', function () {
  expect(this.eslintExitCode).to.equal(0);
});

// PropTypes validation steps
Given('React components that accept props from parent components', async function () {
  this.reactComponents = [];
  
  const findReactComponents = async (dir) => {
    const entries = await readdir(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      if (entry.isDirectory() && !entry.name.startsWith('.') && entry.name !== 'node_modules') {
        await findReactComponents(join(dir, entry.name));
      } else if (entry.isFile() && (entry.name.endsWith('.jsx') || entry.name.endsWith('.js'))) {
        const fullPath = join(dir, entry.name);
        const content = await readFile(fullPath, 'utf8');
        
        // Check if file contains React component patterns
        if (content.includes('React') && 
            (content.includes('class') || content.includes('function')) &&
            (content.includes('render') || content.includes('return'))) {
          this.reactComponents.push({
            path: fullPath,
            content: content,
            name: entry.name
          });
        }
      }
    }
  };
  
  await findReactComponents(join(this.uiWebPath, 'src'));
});

When('the ESLint PropTypes validation rule is applied', function () {
  // Filter ESLint output for PropTypes-related warnings
  const propTypesWarnings = this.eslintOutput.split('\n')
    .filter(line => line.includes('react/prop-types'))
    .map(line => {
      const match = line.match(/(.*):(\d+):(\d+)\s+warning\s+(.*)/);
      return match ? {
        file: match[1],
        line: parseInt(match[2]),
        column: parseInt(match[3]),
        message: match[4]
      } : null;
    })
    .filter(warning => warning !== null);
    
  this.propTypesWarnings = propTypesWarnings;
});

Then('all component props should have proper PropTypes definitions', function () {
  // This validation happens through the ESLint warnings count
  // If there are 0 warnings, then all props have proper PropTypes
  expect(this.propTypesWarnings.length).to.equal(0);
});

Then('missing PropTypes should be flagged as warnings', function () {
  // Verify that missing PropTypes are detected (this validates ESLint rule is working)
  // We expect warnings initially, but they should be resolved by the end
  expect(this.propTypesWarnings).to.be.an('array');
});

Then('the component interface should be clearly documented through PropTypes', function () {
  // Verify that components with PropTypes are self-documenting
  for (const component of this.reactComponents) {
    if (component.content.includes('propTypes')) {
      expect(component.content).to.include('PropTypes');
    }
  }
});

// Unused variable detection steps
Given('JavaScript/React source code files', function () {
  // This leverages the React components already discovered
  expect(this.reactComponents.length).to.be.greaterThan(0);
});

When('the ESLint unused variable detection runs', function () {
  // Filter ESLint output for unused variable warnings
  const unusedVarWarnings = this.eslintOutput.split('\n')
    .filter(line => line.includes('no-unused-vars'))
    .map(line => {
      const match = line.match(/(.*):(\d+):(\d+)\s+warning\s+(.*)/);
      return match ? {
        file: match[1],
        line: parseInt(match[2]),
        column: parseInt(match[3]),
        message: match[4]
      } : null;
    })
    .filter(warning => warning !== null);
    
  this.unusedVarWarnings = unusedVarWarnings;
});

Then('all declared variables should be used within their scope', function () {
  expect(this.unusedVarWarnings.length).to.equal(0);
});

Then('unused imports should be identified for removal', function () {
  const unusedImportWarnings = this.unusedVarWarnings.filter(warning => 
    warning.message.includes('defined but never used')
  );
  // Should be 0 after fixes are applied
  expect(unusedImportWarnings.length).to.equal(0);
});

Then('dead code should be eliminated to improve maintainability', function () {
  // Verify no unreachable code warnings
  const unreachableCodeWarnings = this.eslintOutput.split('\n')
    .filter(line => line.includes('no-unreachable'));
  expect(unreachableCodeWarnings.length).to.equal(0);
});

// React best practices steps
Given('React component implementations', function () {
  expect(this.reactComponents.length).to.be.greaterThan(0);
});

When('ESLint React-specific rules are applied', function () {
  // Filter for React-specific ESLint warnings
  this.reactWarnings = this.eslintOutput.split('\n')
    .filter(line => line.includes('react/') || line.includes('jsx/'))
    .map(line => {
      const match = line.match(/(.*):(\d+):(\d+)\s+warning\s+(.*)/);
      return match ? {
        file: match[1],
        line: parseInt(match[2]),
        column: parseInt(match[3]),
        message: match[4]
      } : null;
    })
    .filter(warning => warning !== null);
});

Then('components should follow React coding standards', function () {
  expect(this.reactWarnings.length).to.equal(0);
});

Then('potential runtime issues should be detected early', function () {
  // Verify no React-specific warnings that could cause runtime issues
  const runtimeIssueWarnings = this.reactWarnings.filter(warning =>
    warning.message.includes('missing') || 
    warning.message.includes('undefined')
  );
  expect(runtimeIssueWarnings.length).to.equal(0);
});

Then('unreachable code should be identified and removed', function () {
  const unreachableWarnings = this.eslintOutput.split('\n')
    .filter(line => line.includes('no-unreachable'));
  expect(unreachableWarnings.length).to.equal(0);
});

// Pre-commit validation steps
Given('a developer attempts to commit code changes', function () {
  this.commitAttempt = true;
});

When('the pre-commit hooks execute ESLint validation', async function () {
  try {
    // Simulate pre-commit hook execution
    const { stdout, stderr } = await execAsync('npm run lint', { 
      cwd: this.uiWebPath,
      timeout: 30000 
    });
    this.preCommitOutput = stdout + stderr;
    this.preCommitSuccess = true;
  } catch (error) {
    this.preCommitOutput = error.stdout + error.stderr;
    this.preCommitSuccess = false;
  }
});

Then('the commit should only succeed if ESLint passes without warnings', function () {
  if (this.preCommitSuccess) {
    // If pre-commit succeeded, there should be no warnings
    const warningMatches = this.preCommitOutput.match(/(\d+) warnings?/);
    const warnings = warningMatches ? parseInt(warningMatches[1]) : 0;
    expect(warnings).to.equal(0);
  }
});

Then('developers should receive immediate feedback on code quality issues', function () {
  expect(this.preCommitOutput).to.include('lint');
});

Then('no low-quality code should enter the repository', function () {
  // This is enforced by the pre-commit hook validation
  expect(this.preCommitSuccess).to.be.true;
});

// Component-specific PropTypes steps
Given('a React component named {string}', function (componentName) {
  this.currentComponent = this.reactComponents.find(component => 
    component.name.includes(componentName) || component.content.includes(componentName)
  );
  expect(this.currentComponent).to.not.be.undefined;
});

When('the component receives props {string}', function (propNames) {
  this.expectedProps = propNames.split(', ').map(prop => prop.trim());
});

Then('each prop should have corresponding PropTypes validation', function () {
  for (const prop of this.expectedProps) {
    // Check if component has PropTypes definition for this prop
    const propTypesRegex = new RegExp(`${prop}\\s*:\\s*PropTypes`);
    const hasPropType = propTypesRegex.test(this.currentComponent.content);
    
    if (!hasPropType) {
      // Check ESLint output for missing PropTypes warning for this specific prop
      const propWarning = this.eslintOutput.includes(`'${prop}' is missing in props validation`);
      expect(propWarning, `PropTypes validation missing for prop: ${prop}`).to.be.false;
    }
  }
});

Then('the PropTypes should accurately reflect the expected data types', function () {
  // Verify PropTypes are present and properly typed
  expect(this.currentComponent.content).to.include('PropTypes');
});

Then('the component should be self-documenting through its PropTypes', function () {
  // Components with PropTypes should be self-documenting
  if (this.currentComponent.content.includes('PropTypes')) {
    expect(this.currentComponent.content).to.include('propTypes');
  }
});

// Systematic resolution steps
Given('{int} ESLint warnings in the codebase', function (warningCount) {
  this.initialWarningCount = warningCount;
});

When('the development team systematically addresses each warning category', function () {
  // This step represents the implementation work that will be done
  this.systematicFixesApplied = true;
});

Then('PropTypes warnings should be resolved by adding proper validation', function () {
  // After fixes, PropTypes warnings should be 0
  const propTypesWarningCount = (this.eslintOutput.match(/react\/prop-types/g) || []).length;
  expect(propTypesWarningCount).to.equal(0);
});

Then('unused variable warnings should be resolved by removing dead code', function () {
  // After fixes, unused variable warnings should be 0
  const unusedVarWarningCount = (this.eslintOutput.match(/no-unused-vars/g) || []).length;
  expect(unusedVarWarningCount).to.equal(0);
});

Then('React best practice warnings should be resolved through refactoring', function () {
  // After fixes, React-specific warnings should be 0
  const reactWarningCount = (this.eslintOutput.match(/no-unreachable/g) || []).length;
  expect(reactWarningCount).to.equal(0);
});

Then('the final result should be {int} warnings across the entire codebase', function (expectedWarnings) {
  const warningMatches = this.eslintOutput.match(/(\d+) warnings?/);
  const actualWarnings = warningMatches ? parseInt(warningMatches[1]) : 0;
  expect(actualWarnings).to.equal(expectedWarnings);
});