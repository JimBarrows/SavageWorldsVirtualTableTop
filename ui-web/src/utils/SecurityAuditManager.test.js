import SecurityAuditManager from './SecurityAuditManager';
import { execSync } from 'child_process';
import fs from 'fs';

// Mock child_process and fs modules
jest.mock('child_process');
jest.mock('fs');

describe('SecurityAuditManager - Supporting BDD Security Scenarios', () => {
  let securityAuditManager;
  
  beforeEach(() => {
    securityAuditManager = new SecurityAuditManager();
    jest.clearAllMocks();
  });

  describe('Security Audit Execution - Supporting Critical Vulnerability Detection', () => {
    test('should execute npm audit and parse vulnerability data', () => {
      const mockAuditOutput = JSON.stringify({
        metadata: {
          vulnerabilities: {
            critical: 13,
            high: 32,
            moderate: 104,
            low: 4,
            total: 153
          }
        }
      });
      
      execSync.mockReturnValueOnce(mockAuditOutput);
      
      const auditResult = securityAuditManager.runSecurityAudit();
      
      expect(execSync).toHaveBeenCalledWith('npm audit --json', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      expect(auditResult.metadata.vulnerabilities.critical).toBe(13);
      expect(auditResult.metadata.vulnerabilities.high).toBe(32);
      expect(auditResult.metadata.vulnerabilities.total).toBe(153);
    });

    test('should handle npm audit command failures gracefully', () => {
      const mockError = new Error('Command failed');
      mockError.stdout = JSON.stringify({
        metadata: {
          vulnerabilities: {
            critical: 5,
            high: 10,
            moderate: 20,
            low: 2,
            total: 37
          }
        }
      });
      
      execSync.mockImplementationOnce(() => {
        throw mockError;
      });
      
      const auditResult = securityAuditManager.runSecurityAudit();
      
      expect(auditResult.metadata.vulnerabilities.total).toBe(37);
    });
  });

  describe('Vulnerability Classification - Supporting BDD Severity Management', () => {
    test('should correctly identify critical vulnerabilities requiring immediate resolution', () => {
      const auditData = {
        metadata: {
          vulnerabilities: {
            critical: 5,
            high: 10,
            moderate: 20,
            low: 3,
            total: 38
          }
        }
      };
      
      const hasCritical = securityAuditManager.hasCriticalVulnerabilities(auditData);
      const criticalCount = securityAuditManager.getCriticalVulnerabilityCount(auditData);
      
      expect(hasCritical).toBe(true);
      expect(criticalCount).toBe(5);
    });

    test('should correctly identify high severity vulnerabilities requiring resolution', () => {
      const auditData = {
        metadata: {
          vulnerabilities: {
            critical: 0,
            high: 15,
            moderate: 25,
            low: 2,
            total: 42
          }
        }
      };
      
      const hasHigh = securityAuditManager.hasHighVulnerabilities(auditData);
      const highCount = securityAuditManager.getHighVulnerabilityCount(auditData);
      
      expect(hasHigh).toBe(true);
      expect(highCount).toBe(15);
    });

    test('should handle zero vulnerabilities state correctly', () => {
      const cleanAuditData = {
        metadata: {
          vulnerabilities: {
            critical: 0,
            high: 0,
            moderate: 0,
            low: 0,
            total: 0
          }
        }
      };
      
      expect(securityAuditManager.hasCriticalVulnerabilities(cleanAuditData)).toBe(false);
      expect(securityAuditManager.hasHighVulnerabilities(cleanAuditData)).toBe(false);
      expect(securityAuditManager.getTotalVulnerabilityCount(cleanAuditData)).toBe(0);
    });
  });

  describe('Security Remediation Actions - Supporting BDD Fix Implementation', () => {
    test('should execute npm audit fix for automatic vulnerability resolution', () => {
      execSync.mockReturnValueOnce('fixed 5 vulnerabilities');
      
      const fixResult = securityAuditManager.executeAutomaticFixes();
      
      expect(execSync).toHaveBeenCalledWith('npm audit fix', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      expect(fixResult).toContain('fixed 5 vulnerabilities');
    });

    test('should execute force fixes with caution for breaking changes', () => {
      execSync.mockReturnValueOnce('fixed 15 vulnerabilities with breaking changes');
      
      const forceFixResult = securityAuditManager.executeForceFixesWithCaution();
      
      expect(execSync).toHaveBeenCalledWith('npm audit fix --force', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      expect(forceFixResult).toContain('fixed 15 vulnerabilities');
    });

    test('should handle fix command failures gracefully', () => {
      execSync.mockImplementationOnce(() => {
        throw new Error('Fix failed due to conflicting peer dependencies');
      });
      
      expect(() => {
        securityAuditManager.executeAutomaticFixes();
      }).toThrow('Fix failed due to conflicting peer dependencies');
    });
  });

  describe('Documentation Generation - Supporting BDD Documentation Requirements', () => {
    test('should generate security remediation documentation', () => {
      const beforeAudit = {
        metadata: {
          vulnerabilities: { critical: 13, high: 32, moderate: 104, low: 4, total: 153 }
        }
      };
      
      const afterAudit = {
        metadata: {
          vulnerabilities: { critical: 0, high: 0, moderate: 10, low: 2, total: 12 }
        }
      };
      
      fs.writeFileSync.mockImplementationOnce(() => {});
      
      securityAuditManager.generateRemediationDocumentation(beforeAudit, afterAudit, [
        'Updated react-scripts to resolve critical webpack vulnerabilities',
        'Applied npm audit fix for automatic dependency updates'
      ]);
      
      expect(fs.writeFileSync).toHaveBeenCalledWith(
        expect.stringContaining('SECURITY_REMEDIATION.md'),
        expect.stringContaining('vulnerabilities resolved'),
        'utf8'
      );
    });

    test('should generate risk assessment documentation for remaining vulnerabilities', () => {
      const remainingVulnerabilities = {
        moderate: 5,
        low: 3
      };
      
      const riskJustifications = [
        'Moderate vulnerability in dev dependency with no production impact',
        'Low severity issue in testing framework - acceptable risk for development'
      ];
      
      fs.writeFileSync.mockImplementationOnce(() => {});
      
      securityAuditManager.generateRiskAssessmentDocumentation(
        remainingVulnerabilities,
        riskJustifications
      );
      
      expect(fs.writeFileSync).toHaveBeenCalledWith(
        expect.stringContaining('SECURITY_RISK_ASSESSMENT.md'),
        expect.stringContaining('business justification'),
        'utf8'
      );
    });
  });

  describe('Build and Test Integration - Supporting BDD Regression Prevention', () => {
    test('should verify application build succeeds after security updates', () => {
      execSync.mockReturnValueOnce('Build completed successfully');
      
      const buildSuccess = securityAuditManager.verifyBuildAfterSecurityUpdates();
      
      expect(execSync).toHaveBeenCalledWith('npm run build', {
        encoding: 'utf8',
        stdio: 'pipe',
        timeout: 300000
      });
      expect(buildSuccess).toBe(true);
    });

    test('should verify unit tests pass after dependency updates', () => {
      execSync.mockReturnValueOnce('All tests passed');
      
      const testsPass = securityAuditManager.verifyTestSuiteAfterUpdates();
      
      expect(execSync).toHaveBeenCalledWith('npm run test:unit -- --watchAll=false', {
        encoding: 'utf8',
        stdio: 'pipe'
      });
      expect(testsPass).toBe(true);
    });

    test('should handle build failures after security updates', () => {
      execSync.mockImplementationOnce(() => {
        throw new Error('Build failed due to dependency conflicts');
      });
      
      const buildSuccess = securityAuditManager.verifyBuildAfterSecurityUpdates();
      
      expect(buildSuccess).toBe(false);
    });
  });

  describe('CI/CD Pipeline Compliance - Supporting BDD Pipeline Validation', () => {
    test('should validate security audit compliance for CI/CD pipeline', () => {
      const auditData = {
        metadata: {
          vulnerabilities: {
            critical: 0,
            high: 0,
            moderate: 5,
            low: 2,
            total: 7
          }
        }
      };
      
      const isCompliant = securityAuditManager.validateCICDSecurityCompliance(auditData);
      
      expect(isCompliant).toBe(true); // Critical and high are zero
    });

    test('should fail compliance when critical or high vulnerabilities exist', () => {
      const nonCompliantAudit = {
        metadata: {
          vulnerabilities: {
            critical: 1,
            high: 3,
            moderate: 10,
            low: 5,
            total: 19
          }
        }
      };
      
      const isCompliant = securityAuditManager.validateCICDSecurityCompliance(nonCompliantAudit);
      
      expect(isCompliant).toBe(false);
    });

    test('should generate compliance report for CI/CD pipeline', () => {
      const auditData = {
        metadata: {
          vulnerabilities: {
            critical: 0,
            high: 0,
            moderate: 3,
            low: 1,
            total: 4
          }
        }
      };
      
      fs.writeFileSync.mockImplementationOnce(() => {});
      
      securityAuditManager.generateComplianceReport(auditData);
      
      expect(fs.writeFileSync).toHaveBeenCalledWith(
        expect.stringContaining('security-compliance-report.json'),
        expect.stringContaining('vulnerabilities'),
        'utf8'
      );
    });
  });

  describe('Baseline Comparison - Supporting BDD Progress Measurement', () => {
    test('should calculate vulnerability reduction percentage', () => {
      const baseline = { total: 153, critical: 13, high: 32 };
      const current = { total: 12, critical: 0, high: 0 };
      
      const reductionStats = securityAuditManager.calculateVulnerabilityReduction(baseline, current);
      
      expect(reductionStats.totalReduction).toBeGreaterThan(90); // >90% reduction
      expect(reductionStats.criticalReduction).toBe(100); // 100% critical resolved
      expect(reductionStats.highReduction).toBe(100); // 100% high resolved
    });

    test('should identify significant improvement in security posture', () => {
      const baseline = { critical: 13, high: 32, moderate: 104 };
      const current = { critical: 0, high: 0, moderate: 15 };
      
      const hasSignificantImprovement = securityAuditManager.hasSignificantSecurityImprovement(
        baseline,
        current
      );
      
      expect(hasSignificantImprovement).toBe(true);
    });
  });
});