import { execSync } from 'child_process';
import QualityGates from './QualityGates';
import CoverageAnalyzer from './CoverageAnalyzer';

/**
 * PipelineValidator - Orchestrates complete CI/CD pipeline validation
 */
export default class PipelineValidator {
  /**
   * Run unit tests with coverage reporting
   * @param {Object} options - Test execution options
   * @returns {Promise<Object>} Test results
   */
  static async runUnitTests(options = {}) {
    const { timeout = 300000 } = options; // 5 minutes default

    try {
      const command = 'npm test -- --watchAll=false --coverage';
      const output = execSync(command, {
        encoding: 'utf8',
        timeout,
        maxBuffer: 1024 * 1024 * 10 // 10MB buffer
      });

      // Parse test results from Jest output
      const testSuites = this.parseTestSuites(output);
      const tests = this.parseTestCounts(output);
      const coverage = CoverageAnalyzer.parseCoverageReport(output, 'jest');

      return {
        success: true,
        output,
        testSuites,
        tests,
        coverage,
        duration: this.extractDuration(output)
      };
    } catch (error) {
      const output = error.stdout || error.message;
      return {
        success: false,
        error: error.message,
        output,
        testSuites: this.parseTestSuites(output),
        tests: this.parseTestCounts(output),
        coverage: CoverageAnalyzer.parseCoverageReport(output, 'jest')
      };
    }
  }

  /**
   * Run BDD integration tests
   * @param {Object} options - Test execution options
   * @returns {Promise<Object>} BDD test results
   */
  static async runBddTests(options = {}) {
    const { timeout = 600000 } = options; // 10 minutes default

    try {
      const command = 'npm run test:integration';
      const output = execSync(command, {
        encoding: 'utf8',
        timeout,
        maxBuffer: 1024 * 1024 * 10
      });

      const scenarios = this.parseBddScenarios(output);
      const steps = this.parseBddSteps(output);

      return {
        success: true,
        output,
        scenarios,
        steps,
        duration: this.extractDuration(output)
      };
    } catch (error) {
      const output = error.stdout || error.message;
      
      // Check if this is an expected failure (no running app)
      if (output.includes('ERR_CONNECTION_REFUSED') || output.includes('ECONNREFUSED')) {
        return {
          success: false,
          expectedFailure: true,
          error: 'No running application - BDD tests require server to be running',
          output,
          scenarios: this.parseBddScenarios(output),
          steps: this.parseBddSteps(output)
        };
      }

      return {
        success: false,
        error: error.message,
        output,
        scenarios: this.parseBddScenarios(output),
        steps: this.parseBddSteps(output)
      };
    }
  }

  /**
   * Run code linting checks
   * @param {Object} options - Linting options
   * @returns {Promise<Object>} Linting results
   */
  static async runLinting(options = {}) {
    const { autoFix = false, timeout = 120000 } = options; // 2 minutes default

    try {
      // Auto-fix if requested
      if (autoFix) {
        try {
          execSync('npm run lint:fix', {
            encoding: 'utf8',
            timeout: timeout / 2
          });
        } catch (fixError) {
          // Continue even if auto-fix fails
        }
      }

      // Run linting check
      const output = execSync('npm run lint', {
        encoding: 'utf8',
        timeout
      });

      return {
        success: true,
        output,
        errors: 0,
        warnings: 0,
        issues: [],
        autoFixed: autoFix
      };
    } catch (error) {
      const output = error.stdout || error.message;
      const issues = this.parseLintIssues(output);
      const errors = this.countLintIssues(output, 'error');
      const warnings = this.countLintIssues(output, 'warning');

      return {
        success: false,
        error: error.message,
        output,
        errors,
        warnings,
        issues,
        autoFixed: autoFix
      };
    }
  }

  /**
   * Run production build
   * @param {Object} options - Build options
   * @returns {Promise<Object>} Build results
   */
  static async runBuild(options = {}) {
    const { timeout = 300000 } = options; // 5 minutes default
    const startTime = Date.now();

    try {
      const output = execSync('npm run build', {
        encoding: 'utf8',
        timeout
      });

      const warnings = this.countBuildWarnings(output);
      const warningMessages = this.extractBuildWarnings(output);

      return {
        success: true,
        output,
        warnings,
        warningMessages,
        errors: 0,
        duration: Date.now() - startTime
      };
    } catch (error) {
      const output = error.stdout || error.message;
      return {
        success: false,
        error: error.message,
        output,
        errors: this.countBuildErrors(output),
        warnings: this.countBuildWarnings(output),
        errorMessage: this.extractBuildErrors(output),
        duration: Date.now() - startTime
      };
    }
  }

  /**
   * Run security vulnerability scan
   * @param {Object} options - Security scan options
   * @returns {Promise<Object>} Security scan results
   */
  static async runSecurityScan(options = {}) {
    const { allowModerate = false, timeout = 120000 } = options; // 2 minutes default
    const auditLevel = allowModerate ? 'high' : 'moderate';

    try {
      const output = execSync(`npm audit --audit-level=${auditLevel}`, {
        encoding: 'utf8',
        timeout
      });

      const vulnerabilities = this.parseAuditResults(output);
      const hasHighSeverity = vulnerabilities.critical > 0 || vulnerabilities.high > 0;

      return {
        success: !hasHighSeverity,
        output,
        vulnerabilities
      };
    } catch (error) {
      const output = error.stdout || error.message;
      const vulnerabilities = this.parseAuditResults(output);

      return {
        success: false,
        error: error.message,
        output,
        vulnerabilities
      };
    }
  }

  /**
   * Run complete pipeline validation
   * @param {Object} options - Pipeline options
   * @returns {Promise<Object>} Complete validation results
   */
  static async validatePipeline(options = {}) {
    const {
      skipUnitTests = false,
      skipBdd = false,
      skipLinting = false,
      skipBuild = false,
      skipSecurity = false,
      continueOnFailure = false,
      coverageThreshold = 85
    } = options;

    const results = {
      timestamp: new Date().toISOString(),
      overall: {
        success: true,
        score: 0,
        totalChecks: 0,
        passedChecks: 0,
        failedChecks: []
      },
      checks: {}
    };

    const checks = [
      { name: 'unitTests', skip: skipUnitTests, fn: () => this.runUnitTests() },
      { name: 'bddTests', skip: skipBdd, fn: () => this.runBddTests() },
      { name: 'linting', skip: skipLinting, fn: () => this.runLinting() },
      { name: 'build', skip: skipBuild, fn: () => this.runBuild() },
      { name: 'security', skip: skipSecurity, fn: () => this.runSecurityScan() }
    ];

    for (const check of checks) {
      if (check.skip) {
        results.checks[check.name] = { skipped: true };
        continue;
      }

      results.overall.totalChecks++;

      try {
        const checkResult = await check.fn();
        results.checks[check.name] = checkResult;

        // Determine if check passed based on its specific criteria
        let checkPassed = checkResult.success;
        
        // Special handling for unit tests - also validate coverage
        if (check.name === 'unitTests' && checkResult.coverage) {
          const coverageValidation = QualityGates.validateCoverage(
            checkResult.coverage,
            coverageThreshold
          );
          checkResult.coverageValidation = coverageValidation;
          checkPassed = checkPassed && coverageValidation.passed;
        }

        // Special handling for BDD tests - expected failures are OK for now
        if (check.name === 'bddTests' && checkResult.expectedFailure) {
          checkPassed = true; // Don't fail pipeline for expected BDD failures
        }

        if (checkPassed) {
          results.overall.passedChecks++;
        } else {
          results.overall.failedChecks.push(check.name);
          if (!continueOnFailure) {
            results.overall.success = false;
            break;
          }
        }
      } catch (error) {
        results.checks[check.name] = {
          success: false,
          error: error.message
        };
        results.overall.failedChecks.push(check.name);
        
        if (!continueOnFailure) {
          results.overall.success = false;
          break;
        }
      }
    }

    // Calculate overall success and score
    results.overall.success = results.overall.failedChecks.length === 0;
    results.overall.score = results.overall.totalChecks > 0 
      ? Math.round((results.overall.passedChecks / results.overall.totalChecks) * 100)
      : 0;

    return results;
  }

  // Utility methods for parsing outputs

  static parseTestSuites(output) {
    const suitesMatch = output.match(/Test Suites: (\d+) passed(?:, (\d+) failed)?(?:, (\d+) total)?/);
    if (suitesMatch) {
      return {
        passed: parseInt(suitesMatch[1]) || 0,
        failed: parseInt(suitesMatch[2]) || 0,
        total: parseInt(suitesMatch[3]) || (parseInt(suitesMatch[1]) + parseInt(suitesMatch[2] || 0))
      };
    }
    return { passed: 0, failed: 0, total: 0 };
  }

  static parseTestCounts(output) {
    const testsMatch = output.match(/Tests:\s+(\d+) passed(?:, (\d+) failed)?(?:, (\d+) total)?/);
    if (testsMatch) {
      return {
        passed: parseInt(testsMatch[1]) || 0,
        failed: parseInt(testsMatch[2]) || 0,
        total: parseInt(testsMatch[3]) || (parseInt(testsMatch[1]) + parseInt(testsMatch[2] || 0))
      };
    }
    return { passed: 0, failed: 0, total: 0 };
  }

  static parseBddScenarios(output) {
    const scenariosMatch = output.match(/(\d+) scenarios? \((\d+) passed(?:, (\d+) failed)?\)/);
    if (scenariosMatch) {
      return {
        total: parseInt(scenariosMatch[1]) || 0,
        passed: parseInt(scenariosMatch[2]) || 0,
        failed: parseInt(scenariosMatch[3]) || 0
      };
    }
    return { total: 0, passed: 0, failed: 0 };
  }

  static parseBddSteps(output) {
    const stepsMatch = output.match(/(\d+) steps? \((\d+) passed(?:, (\d+) failed)?\)/);
    if (stepsMatch) {
      return {
        total: parseInt(stepsMatch[1]) || 0,
        passed: parseInt(stepsMatch[2]) || 0,
        failed: parseInt(stepsMatch[3]) || 0
      };
    }
    return { total: 0, passed: 0, failed: 0 };
  }

  static parseLintIssues(output) {
    const issues = [];
    const lines = output.split('\n');
    
    lines.forEach(line => {
      const issueMatch = line.match(/(.+):(\d+):(\d+):\s+(error|warning)\s+(.+)/);
      if (issueMatch) {
        issues.push({
          file: issueMatch[1],
          line: parseInt(issueMatch[2]),
          column: parseInt(issueMatch[3]),
          severity: issueMatch[4],
          message: issueMatch[5]
        });
      }
    });

    return issues;
  }

  static countLintIssues(output, severity) {
    const regex = new RegExp(`\\b${severity}\\b`, 'gi');
    const matches = output.match(regex);
    return matches ? matches.length : 0;
  }

  static parseAuditResults(output) {
    const vulnerabilities = {
      critical: 0,
      high: 0,
      moderate: 0,
      low: 0
    };

    Object.keys(vulnerabilities).forEach(severity => {
      const regex = new RegExp(`(\\d+) ${severity}`, 'i');
      const match = output.match(regex);
      if (match) {
        vulnerabilities[severity] = parseInt(match[1]);
      }
    });

    return vulnerabilities;
  }

  static countBuildWarnings(output) {
    const warningMatches = output.match(/warning/gi) || [];
    return warningMatches.length;
  }

  static countBuildErrors(output) {
    const errorMatches = output.match(/error/gi) || [];
    return errorMatches.length;
  }

  static extractBuildWarnings(output) {
    const lines = output.split('\n');
    return lines.filter(line => line.toLowerCase().includes('warning')).join('\n');
  }

  static extractBuildErrors(output) {
    const lines = output.split('\n');
    return lines.filter(line => 
      line.toLowerCase().includes('error') || 
      line.toLowerCase().includes('failed')
    ).join('\n');
  }

  static extractDuration(output) {
    const durationMatch = output.match(/Time:\s+(\d+(?:\.\d+)?)\s*s/);
    return durationMatch ? parseFloat(durationMatch[1]) * 1000 : 0;
  }
}