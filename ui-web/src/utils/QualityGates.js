/**
 * QualityGates - Validates various quality metrics and generates reports
 */
export default class QualityGates {
  /**
   * Validate code coverage against minimum thresholds
   * @param {Object} coverage - Coverage metrics {statements, branches, functions, lines}
   * @param {number} minThreshold - Minimum percentage threshold (default 85)
   * @returns {Object} Validation result {passed, message}
   */
  static validateCoverage(coverage, minThreshold = 85) {
    if (!coverage || typeof coverage !== 'object') {
      return {
        passed: false,
        message: 'Invalid coverage data provided'
      };
    }

    const metrics = ['statements', 'branches', 'functions', 'lines'];
    const failures = [];

    for (const metric of metrics) {
      const value = coverage[metric];
      if (value === undefined || value === null) {
        return {
          passed: false,
          message: 'Invalid coverage data: missing required metrics'
        };
      }

      if (value < minThreshold) {
        const metricName = metric.charAt(0).toUpperCase() + metric.slice(1);
        failures.push(`${metricName} coverage ${value}% is below minimum ${minThreshold}%`);
      }
    }

    if (failures.length > 0) {
      return {
        passed: false,
        message: failures.join(', ')
      };
    }

    return {
      passed: true,
      message: `Coverage validation passed. All metrics meet or exceed ${minThreshold}% threshold.`
    };
  }

  /**
   * Validate test execution results
   * @param {Object} testResults - Test results {total, passed, failed, skipped}
   * @returns {Object} Validation result {passed, message}
   */
  static validateTestResults(testResults) {
    const { total, passed, failed, skipped } = testResults;

    if (total === 0) {
      return {
        passed: false,
        message: 'No tests found - test suite appears to be empty'
      };
    }

    if (failed > 0) {
      return {
        passed: false,
        message: `Test validation failed: ${failed} failed tests out of ${total} total`
      };
    }

    let message = `All tests passed: ${passed}/${total} tests successful`;
    if (skipped > 0) {
      message += ` (${skipped} skipped)`;
    }

    return {
      passed: true,
      message
    };
  }

  /**
   * Validate linting results
   * @param {Object} lintResults - Linting results {errors, warnings, files}
   * @returns {Object} Validation result {passed, message}
   */
  static validateLinting(lintResults) {
    const { errors, warnings, files } = lintResults;

    if (errors === 0 && warnings === 0) {
      return {
        passed: true,
        message: `No linting issues found across ${files || 'all'} files`
      };
    }

    const issues = [];
    if (errors > 0) {
      issues.push(`${errors} linting errors`);
    }
    if (warnings > 0) {
      issues.push(`${warnings} linting warnings`);
    }

    return {
      passed: false,
      message: `Linting validation failed: ${issues.join(' and ')} found`
    };
  }

  /**
   * Validate security scan results
   * @param {Object} securityResults - Security scan results {critical, high, moderate, low}
   * @returns {Object} Validation result {passed, message}
   */
  static validateSecurityScan(securityResults) {
    const { critical, high, moderate, low } = securityResults;

    if (critical === 0 && high === 0) {
      let message = 'No critical or high vulnerabilities found';
      if (moderate > 0 || low > 0) {
        const lowerSeverity = [];
        if (moderate > 0) lowerSeverity.push(`${moderate} moderate`);
        if (low > 0) lowerSeverity.push(`${low} low`);
        message += ` (${lowerSeverity.join(', ')} severity issues present)`;
      }

      return {
        passed: true,
        message
      };
    }

    const criticalIssues = [];
    if (critical > 0) {
      criticalIssues.push(`${critical} critical vulnerabilities`);
    }
    if (high > 0) {
      criticalIssues.push(`${high} high vulnerabilities`);
    }

    return {
      passed: false,
      message: `Security validation failed: ${criticalIssues.join(' and ')} found`
    };
  }

  /**
   * Validate build results
   * @param {Object} buildResults - Build results {success, warnings, errors, duration, errorMessage}
   * @returns {Object} Validation result {passed, message}
   */
  static validateBuildResults(buildResults) {
    const { success, warnings, errors, duration, errorMessage } = buildResults;

    if (!success) {
      let message = 'Build failed';
      if (errors > 0) {
        message += ` with ${errors} errors`;
      }
      if (errorMessage) {
        message += `: ${errorMessage}`;
      }

      return {
        passed: false,
        message
      };
    }

    let message = 'Build completed successfully';
    if (duration) {
      message += ` in ${(duration / 1000).toFixed(2)}s`;
    }
    if (warnings > 0) {
      message += ` (${warnings} warnings)`;
    }

    return {
      passed: true,
      message
    };
  }

  /**
   * Generate comprehensive quality report
   * @param {Object} results - All validation results
   * @returns {Object} Quality report with overall status and details
   */
  static generateQualityReport(results) {
    const checks = Object.keys(results);
    const passedChecks = checks.filter(check => results[check].passed);
    const failedChecks = checks.filter(check => !results[check].passed);
    
    const overall = {
      passed: failedChecks.length === 0,
      score: Math.round((passedChecks.length / checks.length) * 100),
      totalChecks: checks.length,
      passedChecks: passedChecks.length,
      failedChecks: failedChecks
    };

    return {
      timestamp: new Date().toISOString(),
      overall,
      details: results
    };
  }
}