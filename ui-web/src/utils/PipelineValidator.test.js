import PipelineValidator from './PipelineValidator';
import { execSync } from 'child_process';

// Mock child_process
jest.mock('child_process', () => ({
  execSync: jest.fn()
}));

describe('PipelineValidator', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('runUnitTests', () => {
    it('should execute unit tests successfully', async () => {
      const mockOutput = `
Test Suites: 5 passed, 5 total
Tests:       25 passed, 25 total
Snapshots:   0 total
Time:        3.456 s
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runUnitTests();
      
      expect(result.success).toBe(true);
      expect(result.testSuites.passed).toBe(5);
      expect(result.testSuites.total).toBe(5);
      expect(result.tests.passed).toBe(25);
      expect(result.tests.total).toBe(25);
      expect(execSync).toHaveBeenCalledWith('npm test -- --watchAll=false --coverage', expect.any(Object));
    });

    it('should handle test failures', async () => {
      const mockOutput = `
Test Suites: 3 passed, 2 failed, 5 total
Tests:       20 passed, 5 failed, 25 total
      `;
      
      const error = new Error('Tests failed');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runUnitTests();
      
      expect(result.success).toBe(false);
      expect(result.testSuites.failed).toBe(2);
      expect(result.tests.failed).toBe(5);
      expect(result.error).toContain('Tests failed');
    });

    it('should handle timeout gracefully', async () => {
      execSync.mockImplementation(() => {
        throw new Error('Command timed out');
      });
      
      const result = await PipelineValidator.runUnitTests({ timeout: 1000 });
      
      expect(result.success).toBe(false);
      expect(result.error).toContain('Command timed out');
    });
  });

  describe('runBddTests', () => {
    it('should execute BDD tests successfully', async () => {
      const mockOutput = `
5 scenarios (5 passed)
25 steps (25 passed)
0m05.123s
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runBddTests();
      
      expect(result.success).toBe(true);
      expect(result.scenarios.passed).toBe(5);
      expect(result.scenarios.total).toBe(5);
      expect(result.steps.passed).toBe(25);
      expect(execSync).toHaveBeenCalledWith('npm run test:integration', expect.any(Object));
    });

    it('should handle BDD test failures', async () => {
      const mockOutput = `
5 scenarios (3 passed, 2 failed)
25 steps (20 passed, 5 failed)
      `;
      
      const error = new Error('BDD tests failed');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runBddTests();
      
      expect(result.success).toBe(false);
      expect(result.scenarios.failed).toBe(2);
      expect(result.steps.failed).toBe(5);
    });

    it('should handle connection refused errors as expected', async () => {
      const mockOutput = `
Error: ERR_CONNECTION_REFUSED
      `;
      
      const error = new Error('Connection refused');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runBddTests();
      
      expect(result.success).toBe(false);
      expect(result.expectedFailure).toBe(true);
      expect(result.error).toContain('No running application');
    });
  });

  describe('runLinting', () => {
    it('should pass with no linting issues', async () => {
      const mockOutput = `
✨  No linting errors or warnings found!
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runLinting();
      
      expect(result.success).toBe(true);
      expect(result.errors).toBe(0);
      expect(result.warnings).toBe(0);
      expect(execSync).toHaveBeenCalledWith('npm run lint', expect.any(Object));
    });

    it('should detect linting errors and warnings', async () => {
      const mockOutput = `
/src/App.js
  10:5  error    'React' is defined but never used  no-unused-vars
  15:2  warning  Missing semicolon                   semi

✖ 2 problems (1 error, 1 warning)
      `;
      
      const error = new Error('Linting failed');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runLinting();
      
      expect(result.success).toBe(false);
      expect(result.errors).toBe(1);
      expect(result.warnings).toBe(1);
      expect(result.issues).toHaveLength(2);
    });

    it('should auto-fix linting issues when requested', async () => {
      execSync
        .mockReturnValueOnce('Fixed 3 issues automatically') // lint:fix
        .mockReturnValueOnce('✨  No linting errors or warnings found!'); // lint check
      
      const result = await PipelineValidator.runLinting({ autoFix: true });
      
      expect(result.success).toBe(true);
      expect(result.autoFixed).toBe(true);
      expect(execSync).toHaveBeenCalledWith('npm run lint:fix', expect.any(Object));
      expect(execSync).toHaveBeenCalledWith('npm run lint', expect.any(Object));
    });
  });

  describe('runBuild', () => {
    it('should build successfully', async () => {
      const mockOutput = `
Creating an optimized production build...
Compiled successfully in 45.67s

File sizes after gzip:
  123.45 kB  build/static/js/main.abc123.js
  67.89 kB   build/static/css/main.def456.css
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runBuild();
      
      expect(result.success).toBe(true);
      expect(result.warnings).toBe(0);
      expect(result.duration).toBeGreaterThan(0);
      expect(execSync).toHaveBeenCalledWith('npm run build', expect.any(Object));
    });

    it('should handle build failures', async () => {
      const mockOutput = `
Failed to compile.

./src/App.js
Module not found: Can't resolve './nonexistent'
      `;
      
      const error = new Error('Build failed');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runBuild();
      
      expect(result.success).toBe(false);
      expect(result.errors).toBeGreaterThan(0);
      expect(result.errorMessage).toContain('Module not found');
    });

    it('should detect build warnings', async () => {
      const mockOutput = `
Compiled with warnings.

Warning in ./src/App.js
Line 10:5: 'React' is defined but never used
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runBuild();
      
      expect(result.success).toBe(true);
      expect(result.warnings).toBeGreaterThan(0);
      expect(result.warningMessages).toContain("'React' is defined but never used");
    });
  });

  describe('runSecurityScan', () => {
    it('should pass security scan with no vulnerabilities', async () => {
      const mockOutput = `
found 0 vulnerabilities
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runSecurityScan();
      
      expect(result.success).toBe(true);
      expect(result.vulnerabilities.critical).toBe(0);
      expect(result.vulnerabilities.high).toBe(0);
      expect(execSync).toHaveBeenCalledWith('npm audit --audit-level=moderate', expect.any(Object));
    });

    it('should detect security vulnerabilities', async () => {
      const mockOutput = `
found 15 vulnerabilities (2 low, 5 moderate, 6 high, 2 critical)
      `;
      
      const error = new Error('Vulnerabilities found');
      error.stdout = mockOutput;
      execSync.mockImplementation(() => {
        throw error;
      });
      
      const result = await PipelineValidator.runSecurityScan();
      
      expect(result.success).toBe(false);
      expect(result.vulnerabilities.low).toBe(2);
      expect(result.vulnerabilities.moderate).toBe(5);
      expect(result.vulnerabilities.high).toBe(6);
      expect(result.vulnerabilities.critical).toBe(2);
    });

    it('should allow moderate vulnerabilities when configured', async () => {
      const mockOutput = `
found 5 vulnerabilities (2 low, 3 moderate)
      `;
      
      execSync.mockReturnValue(mockOutput);
      
      const result = await PipelineValidator.runSecurityScan({ allowModerate: true });
      
      expect(result.success).toBe(true);
      expect(result.vulnerabilities.moderate).toBe(3);
    });
  });

  describe('validatePipeline', () => {
    it('should run full pipeline validation successfully', async () => {
      // Mock all commands to succeed
      execSync
        .mockReturnValueOnce('Test Suites: 5 passed, 5 total\nTests: 25 passed, 25 total') // unit tests
        .mockReturnValueOnce('5 scenarios (5 passed)\n25 steps (25 passed)') // bdd tests
        .mockReturnValueOnce('✨  No linting errors found!') // linting
        .mockReturnValueOnce('Compiled successfully') // build
        .mockReturnValueOnce('found 0 vulnerabilities'); // security
      
      const result = await PipelineValidator.validatePipeline();
      
      expect(result.overall.success).toBe(true);
      expect(result.overall.score).toBe(100);
      expect(result.checks.unitTests.success).toBe(true);
      expect(result.checks.bddTests.success).toBe(true);
      expect(result.checks.linting.success).toBe(true);
      expect(result.checks.build.success).toBe(true);
      expect(result.checks.security.success).toBe(true);
    });

    it('should fail overall validation when any check fails', async () => {
      // Mock unit tests to fail, others to succeed
      execSync
        .mockImplementationOnce(() => { throw new Error('Tests failed'); }) // unit tests fail
        .mockReturnValueOnce('5 scenarios (5 passed)') // bdd tests pass
        .mockReturnValueOnce('✨  No linting errors found!') // linting pass
        .mockReturnValueOnce('Compiled successfully') // build pass
        .mockReturnValueOnce('found 0 vulnerabilities'); // security pass
      
      const result = await PipelineValidator.validatePipeline();
      
      expect(result.overall.success).toBe(false);
      expect(result.overall.score).toBe(80); // 4/5 checks passed
      expect(result.overall.failedChecks).toEqual(['unitTests']);
      expect(result.checks.unitTests.success).toBe(false);
    });

    it('should skip checks when configured', async () => {
      const options = {
        skipBdd: true,
        skipSecurity: true
      };
      
      execSync
        .mockReturnValueOnce('Test Suites: 5 passed, 5 total') // unit tests
        .mockReturnValueOnce('✨  No linting errors found!') // linting
        .mockReturnValueOnce('Compiled successfully'); // build
      
      const result = await PipelineValidator.validatePipeline(options);
      
      expect(result.overall.success).toBe(true);
      expect(result.checks.bddTests.skipped).toBe(true);
      expect(result.checks.security.skipped).toBe(true);
      expect(execSync).toHaveBeenCalledTimes(3); // Only 3 checks ran
    });

    it('should continue on failure when configured', async () => {
      const options = {
        continueOnFailure: true
      };
      
      execSync
        .mockImplementationOnce(() => { throw new Error('Tests failed'); }) // unit tests fail
        .mockImplementationOnce(() => { throw new Error('BDD failed'); }) // bdd tests fail
        .mockReturnValueOnce('✨  No linting errors found!') // linting pass
        .mockReturnValueOnce('Compiled successfully') // build pass
        .mockReturnValueOnce('found 0 vulnerabilities'); // security pass
      
      const result = await PipelineValidator.validatePipeline(options);
      
      expect(result.overall.success).toBe(false);
      expect(result.overall.score).toBe(60); // 3/5 checks passed
      expect(result.checks.unitTests.success).toBe(false);
      expect(result.checks.bddTests.success).toBe(false);
      expect(result.checks.linting.success).toBe(true);
      expect(execSync).toHaveBeenCalledTimes(5); // All checks ran despite failures
    });
  });
});