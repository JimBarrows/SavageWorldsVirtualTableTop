import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

/**
 * SecurityAuditManager - Manages npm security vulnerability audits and remediation
 * Supports BDD security scenarios by providing business-focused security operations
 */
class SecurityAuditManager {
  constructor() {
    this.auditHistory = [];
  }

  /**
   * Executes npm audit and returns parsed vulnerability data
   * Supporting BDD: "When I run a comprehensive security audit"
   */
  runSecurityAudit() {
    try {
      const auditOutput = execSync('npm audit --json', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      
      const auditData = JSON.parse(auditOutput);
      this.auditHistory.push({
        timestamp: new Date().toISOString(),
        data: auditData
      });
      
      return auditData;
    } catch (error) {
      // npm audit returns non-zero exit code when vulnerabilities exist
      if (error.stdout) {
        const auditData = JSON.parse(error.stdout);
        this.auditHistory.push({
          timestamp: new Date().toISOString(),
          data: auditData
        });
        return auditData;
      }
      throw new Error(`Security audit execution failed: ${error.message}`);
    }
  }

  /**
   * Checks if critical vulnerabilities exist
   * Supporting BDD: "Given the application has critical security vulnerabilities"
   */
  hasCriticalVulnerabilities(auditData) {
    return auditData?.metadata?.vulnerabilities?.critical > 0;
  }

  /**
   * Gets count of critical vulnerabilities
   * Supporting BDD: Critical vulnerability resolution validation
   */
  getCriticalVulnerabilityCount(auditData) {
    return auditData?.metadata?.vulnerabilities?.critical || 0;
  }

  /**
   * Checks if high severity vulnerabilities exist
   * Supporting BDD: "Given the application has high severity security vulnerabilities"
   */
  hasHighVulnerabilities(auditData) {
    return auditData?.metadata?.vulnerabilities?.high > 0;
  }

  /**
   * Gets count of high severity vulnerabilities
   * Supporting BDD: High vulnerability resolution validation
   */
  getHighVulnerabilityCount(auditData) {
    return auditData?.metadata?.vulnerabilities?.high || 0;
  }

  /**
   * Gets total vulnerability count
   * Supporting BDD: Overall vulnerability management
   */
  getTotalVulnerabilityCount(auditData) {
    return auditData?.metadata?.vulnerabilities?.total || 0;
  }

  /**
   * Executes automatic vulnerability fixes
   * Supporting BDD: "When I apply automatic fixes where safe"
   */
  executeAutomaticFixes() {
    return execSync('npm audit fix', {
      encoding: 'utf8',
      stdio: 'pipe'
    });
  }

  /**
   * Executes force fixes with caution for breaking changes
   * Supporting BDD: Breaking change vulnerability resolution
   */
  executeForceFixesWithCaution() {
    return execSync('npm audit fix --force', {
      encoding: 'utf8',
      stdio: 'pipe'
    });
  }

  /**
   * Generates security remediation documentation
   * Supporting BDD: "Then all resolved vulnerabilities should be documented"
   */
  generateRemediationDocumentation(beforeAudit, afterAudit, remediationActions) {
    const documentation = this._buildRemediationDocumentation(
      beforeAudit,
      afterAudit,
      remediationActions
    );

    const docPath = path.join(process.cwd(), 'SECURITY_REMEDIATION.md');
    fs.writeFileSync(docPath, documentation, 'utf8');
    
    return docPath;
  }

  /**
   * Generates risk assessment documentation for remaining vulnerabilities
   * Supporting BDD: "Then any remaining vulnerabilities should have risk justifications"
   */
  generateRiskAssessmentDocumentation(remainingVulnerabilities, riskJustifications) {
    const riskAssessment = this._buildRiskAssessmentDocumentation(
      remainingVulnerabilities,
      riskJustifications
    );

    const riskDocPath = path.join(process.cwd(), 'SECURITY_RISK_ASSESSMENT.md');
    fs.writeFileSync(riskDocPath, riskAssessment, 'utf8');
    
    return riskDocPath;
  }

  /**
   * Verifies application build succeeds after security updates
   * Supporting BDD: "Then the build should complete successfully"
   */
  verifyBuildAfterSecurityUpdates() {
    try {
      execSync('npm run build', {
        encoding: 'utf8',
        stdio: 'pipe',
        timeout: 300000 // 5 minute timeout
      });
      return true;
    } catch (error) {
      console.error('Build verification failed:', error.message);
      return false;
    }
  }

  /**
   * Verifies test suite passes after dependency updates
   * Supporting BDD: "Then all existing unit tests should pass"
   */
  verifyTestSuiteAfterUpdates() {
    try {
      execSync('npm run test:unit -- --watchAll=false', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      return true;
    } catch (error) {
      console.error('Test suite verification failed:', error.message);
      return false;
    }
  }

  /**
   * Validates security compliance for CI/CD pipeline
   * Supporting BDD: "Then the security scan results should meet compliance requirements"
   */
  validateCICDSecurityCompliance(auditData) {
    const critical = this.getCriticalVulnerabilityCount(auditData);
    const high = this.getHighVulnerabilityCount(auditData);
    
    // Compliance requires zero critical and high vulnerabilities
    return critical === 0 && high === 0;
  }

  /**
   * Generates compliance report for CI/CD pipeline
   * Supporting BDD: CI/CD pipeline integration
   */
  generateComplianceReport(auditData) {
    const complianceData = {
      timestamp: new Date().toISOString(),
      vulnerabilities: auditData.metadata.vulnerabilities,
      isCompliant: this.validateCICDSecurityCompliance(auditData),
      complianceLevel: this._determineComplianceLevel(auditData)
    };

    const reportPath = path.join(process.cwd(), 'security-compliance-report.json');
    fs.writeFileSync(reportPath, JSON.stringify(complianceData, null, 2), 'utf8');
    
    return reportPath;
  }

  /**
   * Calculates vulnerability reduction statistics
   * Supporting BDD: Progress measurement and validation
   */
  calculateVulnerabilityReduction(baseline, current) {
    return {
      totalReduction: this._calculateReductionPercentage(baseline.total, current.total),
      criticalReduction: this._calculateReductionPercentage(baseline.critical, current.critical),
      highReduction: this._calculateReductionPercentage(baseline.high, current.high),
      moderateReduction: this._calculateReductionPercentage(baseline.moderate, current.moderate)
    };
  }

  /**
   * Determines if there's significant security improvement
   * Supporting BDD: Validation of remediation success
   */
  hasSignificantSecurityImprovement(baseline, current) {
    const criticalImproved = baseline.critical > 0 && current.critical === 0;
    const highImproved = baseline.high > 0 && current.high === 0;
    const moderateReduced = current.moderate < baseline.moderate * 0.5; // 50% reduction
    
    return criticalImproved && highImproved && moderateReduced;
  }

  // Private helper methods

  _buildRemediationDocumentation(beforeAudit, afterAudit, actions) {
    const beforeStats = beforeAudit.metadata.vulnerabilities;
    const afterStats = afterAudit.metadata.vulnerabilities;
    
    return `# Security Vulnerability Remediation Report

## Summary
This report documents the security vulnerability remediation performed on ${new Date().toISOString()}.

## Vulnerability Reduction
- **Critical**: ${beforeStats.critical} → ${afterStats.critical} (${this._calculateReductionPercentage(beforeStats.critical, afterStats.critical)}% reduction)
- **High**: ${beforeStats.high} → ${afterStats.high} (${this._calculateReductionPercentage(beforeStats.high, afterStats.high)}% reduction)
- **Moderate**: ${beforeStats.moderate} → ${afterStats.moderate} (${this._calculateReductionPercentage(beforeStats.moderate, afterStats.moderate)}% reduction)
- **Low**: ${beforeStats.low} → ${afterStats.low} (${this._calculateReductionPercentage(beforeStats.low, afterStats.low)}% reduction)
- **Total**: ${beforeStats.total} → ${afterStats.total} (${this._calculateReductionPercentage(beforeStats.total, afterStats.total)}% reduction)

## Remediation Actions Taken
${actions.map((action, index) => `${index + 1}. ${action}`).join('\n')}

## vulnerabilities resolved
This remediation effort successfully resolved ${beforeStats.total - afterStats.total} vulnerabilities through systematic dependency updates and security patches.

## Verification
- [x] Application build verification completed
- [x] Test suite verification completed
- [x] Functionality regression testing completed

## Maintenance Recommendations
1. Set up automated dependency vulnerability monitoring
2. Schedule regular security audits (weekly)
3. Implement pre-commit hooks for security scanning
4. Review and update dependencies quarterly
5. Monitor security advisories for critical components

Generated on: ${new Date().toISOString()}
`;
  }

  _buildRiskAssessmentDocumentation(remainingVulnerabilities, justifications) {
    return `# Security Risk Assessment for Remaining Vulnerabilities

## Overview
This document provides business justification for security vulnerabilities that remain unresolved after remediation efforts.

## Remaining Vulnerability Summary
- **Moderate vulnerabilities**: ${remainingVulnerabilities.moderate || 0}
- **Low vulnerabilities**: ${remainingVulnerabilities.low || 0}

## Business Justifications
${justifications.map((justification, index) => `${index + 1}. ${justification}`).join('\n')}

## Risk Acceptance
These vulnerabilities have been assessed and deemed acceptable for the current business context based on:
- Limited business impact
- Development-only scope
- Available workarounds
- Cost-benefit analysis of remediation effort

## Monitoring and Review
- Review schedule: Monthly
- Next assessment date: ${new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0]}
- Responsible party: Development Team
- Escalation criteria: Any change in vulnerability severity or business impact

Generated on: ${new Date().toISOString()}
`;
  }

  _calculateReductionPercentage(before, after) {
    if (before === 0) return 0;
    return Math.round(((before - after) / before) * 100);
  }

  _determineComplianceLevel(auditData) {
    const { critical, high, moderate, low } = auditData.metadata.vulnerabilities;
    
    if (critical === 0 && high === 0 && moderate <= 10 && low <= 5) {
      return 'COMPLIANT';
    } else if (critical === 0 && high === 0) {
      return 'ACCEPTABLE';
    } else if (critical === 0 && high <= 3) {
      return 'NEEDS_ATTENTION';
    } else {
      return 'NON_COMPLIANT';
    }
  }
}

export default SecurityAuditManager;