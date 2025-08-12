import QualityGates from './QualityGates';

describe('QualityGates', () => {
  describe('validateCoverage', () => {
    it('should pass when coverage meets minimum threshold', () => {
      const coverage = {
        statements: 90,
        branches: 85,
        functions: 88,
        lines: 92
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('Coverage validation passed');
    });

    it('should fail when statements coverage is below threshold', () => {
      const coverage = {
        statements: 80,
        branches: 90,
        functions: 88,
        lines: 92
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Statements coverage');
      expect(result.message).toContain('80%');
      expect(result.message).toContain('below minimum 85%');
    });

    it('should fail when branches coverage is below threshold', () => {
      const coverage = {
        statements: 90,
        branches: 80,
        functions: 88,
        lines: 92
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Branches coverage');
    });

    it('should fail when functions coverage is below threshold', () => {
      const coverage = {
        statements: 90,
        branches: 90,
        functions: 80,
        lines: 92
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Functions coverage');
    });

    it('should fail when lines coverage is below threshold', () => {
      const coverage = {
        statements: 90,
        branches: 90,
        functions: 88,
        lines: 80
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Lines coverage');
    });

    it('should handle edge case where coverage equals threshold', () => {
      const coverage = {
        statements: 85,
        branches: 85,
        functions: 85,
        lines: 85
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(true);
    });

    it('should handle missing coverage data gracefully', () => {
      const coverage = {
        statements: undefined,
        branches: 85,
        functions: 85,
        lines: 85
      };
      
      const result = QualityGates.validateCoverage(coverage, 85);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Invalid coverage data');
    });
  });

  describe('validateTestResults', () => {
    it('should pass when all tests pass', () => {
      const testResults = {
        total: 100,
        passed: 100,
        failed: 0,
        skipped: 0
      };
      
      const result = QualityGates.validateTestResults(testResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('All tests passed');
    });

    it('should fail when there are test failures', () => {
      const testResults = {
        total: 100,
        passed: 95,
        failed: 5,
        skipped: 0
      };
      
      const result = QualityGates.validateTestResults(testResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('5 failed tests');
    });

    it('should pass when there are only skipped tests', () => {
      const testResults = {
        total: 100,
        passed: 90,
        failed: 0,
        skipped: 10
      };
      
      const result = QualityGates.validateTestResults(testResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('All tests passed');
      expect(result.message).toContain('10 skipped');
    });

    it('should handle zero tests case', () => {
      const testResults = {
        total: 0,
        passed: 0,
        failed: 0,
        skipped: 0
      };
      
      const result = QualityGates.validateTestResults(testResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('No tests found');
    });
  });

  describe('validateLinting', () => {
    it('should pass when there are no linting issues', () => {
      const lintResults = {
        errors: 0,
        warnings: 0,
        files: 50
      };
      
      const result = QualityGates.validateLinting(lintResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('No linting issues');
    });

    it('should fail when there are linting errors', () => {
      const lintResults = {
        errors: 5,
        warnings: 0,
        files: 50
      };
      
      const result = QualityGates.validateLinting(lintResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('5 linting errors');
    });

    it('should fail when there are linting warnings', () => {
      const lintResults = {
        errors: 0,
        warnings: 3,
        files: 50
      };
      
      const result = QualityGates.validateLinting(lintResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('3 linting warnings');
    });

    it('should report both errors and warnings', () => {
      const lintResults = {
        errors: 2,
        warnings: 3,
        files: 50
      };
      
      const result = QualityGates.validateLinting(lintResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('2 linting errors');
      expect(result.message).toContain('3 linting warnings');
    });
  });

  describe('validateSecurityScan', () => {
    it('should pass when there are no high or critical vulnerabilities', () => {
      const securityResults = {
        critical: 0,
        high: 0,
        moderate: 2,
        low: 5
      };
      
      const result = QualityGates.validateSecurityScan(securityResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('No critical or high vulnerabilities');
    });

    it('should fail when there are critical vulnerabilities', () => {
      const securityResults = {
        critical: 2,
        high: 0,
        moderate: 1,
        low: 3
      };
      
      const result = QualityGates.validateSecurityScan(securityResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('2 critical vulnerabilities');
    });

    it('should fail when there are high vulnerabilities', () => {
      const securityResults = {
        critical: 0,
        high: 3,
        moderate: 1,
        low: 2
      };
      
      const result = QualityGates.validateSecurityScan(securityResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('3 high vulnerabilities');
    });

    it('should report both critical and high vulnerabilities', () => {
      const securityResults = {
        critical: 1,
        high: 2,
        moderate: 1,
        low: 2
      };
      
      const result = QualityGates.validateSecurityScan(securityResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('1 critical');
      expect(result.message).toContain('2 high');
    });
  });

  describe('validateBuildResults', () => {
    it('should pass when build succeeds', () => {
      const buildResults = {
        success: true,
        warnings: 0,
        errors: 0,
        duration: 30000
      };
      
      const result = QualityGates.validateBuildResults(buildResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('Build completed successfully');
    });

    it('should fail when build fails', () => {
      const buildResults = {
        success: false,
        warnings: 0,
        errors: 2,
        duration: 15000,
        errorMessage: 'Compilation failed'
      };
      
      const result = QualityGates.validateBuildResults(buildResults);
      
      expect(result.passed).toBe(false);
      expect(result.message).toContain('Build failed');
      expect(result.message).toContain('Compilation failed');
    });

    it('should pass build with warnings but note them', () => {
      const buildResults = {
        success: true,
        warnings: 3,
        errors: 0,
        duration: 35000
      };
      
      const result = QualityGates.validateBuildResults(buildResults);
      
      expect(result.passed).toBe(true);
      expect(result.message).toContain('Build completed successfully');
      expect(result.message).toContain('3 warnings');
    });
  });

  describe('generateQualityReport', () => {
    it('should generate comprehensive quality report', () => {
      const results = {
        coverage: { passed: true, message: 'Coverage validation passed' },
        tests: { passed: true, message: 'All tests passed' },
        linting: { passed: true, message: 'No linting issues' },
        security: { passed: true, message: 'No critical vulnerabilities' },
        build: { passed: true, message: 'Build completed successfully' }
      };
      
      const report = QualityGates.generateQualityReport(results);
      
      expect(report.overall.passed).toBe(true);
      expect(report.overall.score).toBe(100);
      expect(report.details).toEqual(results);
      expect(report.timestamp).toBeDefined();
    });

    it('should fail overall when any check fails', () => {
      const results = {
        coverage: { passed: true, message: 'Coverage validation passed' },
        tests: { passed: false, message: '5 failed tests' },
        linting: { passed: true, message: 'No linting issues' },
        security: { passed: true, message: 'No critical vulnerabilities' },
        build: { passed: true, message: 'Build completed successfully' }
      };
      
      const report = QualityGates.generateQualityReport(results);
      
      expect(report.overall.passed).toBe(false);
      expect(report.overall.score).toBe(80); // 4/5 passed = 80%
      expect(report.overall.failedChecks).toEqual(['tests']);
    });

    it('should calculate correct score for multiple failures', () => {
      const results = {
        coverage: { passed: false, message: 'Coverage below threshold' },
        tests: { passed: false, message: '5 failed tests' },
        linting: { passed: true, message: 'No linting issues' },
        security: { passed: true, message: 'No critical vulnerabilities' },
        build: { passed: true, message: 'Build completed successfully' }
      };
      
      const report = QualityGates.generateQualityReport(results);
      
      expect(report.overall.passed).toBe(false);
      expect(report.overall.score).toBe(60); // 3/5 passed = 60%
      expect(report.overall.failedChecks).toEqual(['coverage', 'tests']);
    });
  });
});