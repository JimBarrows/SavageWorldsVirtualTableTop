import fs from 'fs';

/**
 * CoverageAnalyzer - Parses and analyzes code coverage reports
 */
export default class CoverageAnalyzer {
  /**
   * Parse coverage report from various formats
   * @param {string} reportContent - Raw coverage report content
   * @param {string} format - Format type ('jest', 'lcov', 'json')
   * @returns {Object} Parsed coverage metrics {statements, branches, functions, lines}
   */
  static parseCoverageReport(reportContent, format = 'jest') {
    if (!reportContent || typeof reportContent !== 'string') {
      return { statements: 0, branches: 0, functions: 0, lines: 0 };
    }

    switch (format.toLowerCase()) {
      case 'jest':
        return this.parseJestOutput(reportContent);
      case 'lcov':
        return this.parseLcovContent(reportContent);
      case 'json':
        return this.parseJsonCoverage(reportContent);
      default:
        // Fallback to Jest parsing for unknown formats
        return this.parseJestOutput(reportContent);
    }
  }

  /**
   * Parse Jest coverage output
   */
  static parseJestOutput(output) {
    const summaryRegex = /Statements\s*:\s*(\d+\.?\d*)%.*\n.*Branches\s*:\s*(\d+\.?\d*)%.*\n.*Functions\s*:\s*(\d+\.?\d*)%.*\n.*Lines\s*:\s*(\d+\.?\d*)%/;
    const match = output.match(summaryRegex);

    if (match) {
      return {
        statements: parseFloat(match[1]),
        branches: parseFloat(match[2]),
        functions: parseFloat(match[3]),
        lines: parseFloat(match[4])
      };
    }

    // Try alternative format
    const altRegex = /All files\s*\|\s*(\d+\.?\d*)\s*\|\s*(\d+\.?\d*)\s*\|\s*(\d+\.?\d*)\s*\|\s*(\d+\.?\d*)/;
    const altMatch = output.match(altRegex);

    if (altMatch) {
      return {
        statements: parseFloat(altMatch[1]),
        branches: parseFloat(altMatch[2]),
        functions: parseFloat(altMatch[3]),
        lines: parseFloat(altMatch[4])
      };
    }

    return { statements: 0, branches: 0, functions: 0, lines: 0 };
  }

  /**
   * Parse LCOV format coverage
   */
  static parseLcovContent(lcovContent) {
    const records = lcovContent.split('end_of_record');
    let totalFunctions = 0, hitFunctions = 0;
    let totalLines = 0, hitLines = 0;
    let totalBranches = 0, hitBranches = 0;

    records.forEach(record => {
      const fnfMatch = record.match(/FNF:(\d+)/);
      const fnhMatch = record.match(/FNH:(\d+)/);
      const lfMatch = record.match(/LF:(\d+)/);
      const lhMatch = record.match(/LH:(\d+)/);
      const brfMatch = record.match(/BRF:(\d+)/);
      const brhMatch = record.match(/BRH:(\d+)/);

      if (fnfMatch && fnhMatch) {
        totalFunctions += parseInt(fnfMatch[1]);
        hitFunctions += parseInt(fnhMatch[1]);
      }
      if (lfMatch && lhMatch) {
        totalLines += parseInt(lfMatch[1]);
        hitLines += parseInt(lhMatch[1]);
      }
      if (brfMatch && brhMatch) {
        totalBranches += parseInt(brfMatch[1]);
        hitBranches += parseInt(brhMatch[1]);
      }
    });

    return {
      statements: totalLines > 0 ? Math.round((hitLines / totalLines) * 10000) / 100 : 0,
      branches: totalBranches > 0 ? Math.round((hitBranches / totalBranches) * 10000) / 100 : 0,
      functions: totalFunctions > 0 ? Math.round((hitFunctions / totalFunctions) * 10000) / 100 : 0,
      lines: totalLines > 0 ? Math.round((hitLines / totalLines) * 10000) / 100 : 0
    };
  }

  /**
   * Parse JSON coverage format
   */
  static parseJsonCoverage(jsonContent) {
    try {
      const data = JSON.parse(jsonContent);
      return this.calculateOverallCoverage(data);
    } catch (error) {
      return { statements: 0, branches: 0, functions: 0, lines: 0, error: error.message };
    }
  }

  /**
   * Analyze coverage file and return metrics
   * @param {string} filePath - Path to coverage file
   * @returns {Promise<Object>} Coverage analysis results
   */
  static async analyzeCoverageFile(filePath) {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      
      if (filePath.endsWith('.json')) {
        const coverageData = JSON.parse(content);
        return this.calculateOverallCoverage(coverageData);
      } else if (filePath.includes('lcov')) {
        return this.parseLcovContent(content);
      } else {
        return this.parseJestOutput(content);
      }
    } catch (error) {
      return {
        statements: 0,
        branches: 0,
        functions: 0,
        lines: 0,
        error: error.message
      };
    }
  }

  /**
   * Calculate overall coverage from file-level coverage data
   * @param {Object} fileCoverageData - Per-file coverage data
   * @returns {Object} Overall coverage metrics
   */
  static calculateOverallCoverage(fileCoverageData) {
    if (!fileCoverageData || Object.keys(fileCoverageData).length === 0) {
      return { statements: 0, branches: 0, functions: 0, lines: 0 };
    }

    let totalStatements = 0, coveredStatements = 0;
    let totalBranches = 0, coveredBranches = 0;
    let totalFunctions = 0, coveredFunctions = 0;
    let totalLines = 0, coveredLines = 0;

    Object.values(fileCoverageData).forEach(fileData => {
      if (fileData.s && fileData.statementMap) {
        // Istanbul format
        const statements = Object.values(fileData.s);
        totalStatements += statements.length;
        coveredStatements += statements.filter(count => count > 0).length;

        if (fileData.b && fileData.branchMap) {
          const branches = Object.values(fileData.b).flat();
          totalBranches += branches.length;
          coveredBranches += branches.filter(count => count > 0).length;
        }

        if (fileData.f && fileData.fnMap) {
          const functions = Object.values(fileData.f);
          totalFunctions += functions.length;
          coveredFunctions += functions.filter(count => count > 0).length;
        }

        totalLines += statements.length; // Approximation
        coveredLines += statements.filter(count => count > 0).length;
      } else if (fileData.statements) {
        // Alternative format
        totalStatements += fileData.statements.total || 0;
        coveredStatements += fileData.statements.covered || 0;
        totalBranches += fileData.branches.total || 0;
        coveredBranches += fileData.branches.covered || 0;
        totalFunctions += fileData.functions.total || 0;
        coveredFunctions += fileData.functions.covered || 0;
        totalLines += fileData.lines.total || 0;
        coveredLines += fileData.lines.covered || 0;
      }
    });

    return {
      statements: totalStatements > 0 ? Math.round((coveredStatements / totalStatements) * 10000) / 100 : 0,
      branches: totalBranches > 0 ? Math.round((coveredBranches / totalBranches) * 10000) / 100 : 0,
      functions: totalFunctions > 0 ? Math.round((coveredFunctions / totalFunctions) * 10000) / 100 : 0,
      lines: totalLines > 0 ? Math.round((coveredLines / totalLines) * 10000) / 100 : 0
    };
  }

  /**
   * Find uncovered areas in the code
   * @param {Object} coverageData - Detailed coverage data
   * @returns {Object} Uncovered areas by file
   */
  static findUncoveredAreas(coverageData) {
    const uncoveredAreas = {};

    Object.entries(coverageData).forEach(([filePath, fileData]) => {
      const uncovered = {
        uncoveredStatements: [],
        uncoveredBranches: [],
        uncoveredFunctions: []
      };

      // Find uncovered statements
      if (fileData.s && fileData.statementMap) {
        Object.entries(fileData.s).forEach(([id, count]) => {
          if (count === 0 && fileData.statementMap[id]) {
            uncovered.uncoveredStatements.push({
              id,
              location: fileData.statementMap[id]
            });
          }
        });
      }

      // Find uncovered branches
      if (fileData.b && fileData.branchMap) {
        Object.entries(fileData.b).forEach(([id, branches]) => {
          branches.forEach((count, branchIndex) => {
            if (count === 0 && fileData.branchMap[id]) {
              uncovered.uncoveredBranches.push({
                id,
                branchIndex,
                location: fileData.branchMap[id].locations?.[branchIndex]
              });
            }
          });
        });
      }

      // Find uncovered functions
      if (fileData.f && fileData.fnMap) {
        Object.entries(fileData.f).forEach(([id, count]) => {
          if (count === 0 && fileData.fnMap[id]) {
            uncovered.uncoveredFunctions.push({
              id,
              name: fileData.fnMap[id].name,
              location: fileData.fnMap[id].loc
            });
          }
        });
      }

      uncoveredAreas[filePath] = uncovered;
    });

    return uncoveredAreas;
  }

  /**
   * Generate coverage report in various formats
   * @param {Object} coverageData - Coverage data to report
   * @param {string} format - Output format ('html', 'json', 'markdown')
   * @returns {string} Formatted coverage report
   */
  static generateCoverageReport(coverageData, format = 'html') {
    const timestamp = new Date().toISOString();

    switch (format.toLowerCase()) {
      case 'json':
        return JSON.stringify({
          ...coverageData,
          timestamp,
          format: 'json'
        }, null, 2);

      case 'markdown':
        return this.generateMarkdownReport(coverageData, timestamp);

      case 'html':
      default:
        return this.generateHtmlReport(coverageData, timestamp);
    }
  }

  /**
   * Generate HTML coverage report
   */
  static generateHtmlReport(coverageData, timestamp) {
    const { overall, files = {} } = coverageData;

    const getStatusIcon = (value, threshold = 85) => value >= threshold ? '✅' : '❌';
    
    let html = `
<!DOCTYPE html>
<html>
<head>
    <title>Coverage Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .overall { background: #f5f5f5; padding: 20px; margin-bottom: 20px; border-radius: 5px; }
        .metric { display: inline-block; margin: 10px 20px; text-align: center; }
        .metric-value { font-size: 24px; font-weight: bold; }
        .files table { width: 100%; border-collapse: collapse; }
        .files th, .files td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        .files th { background-color: #f2f2f2; }
        .pass { color: green; }
        .fail { color: red; }
    </style>
</head>
<body>
    <h1>Coverage Report</h1>
    <p>Generated: ${timestamp}</p>
    
    <div class="overall">
        <h2>Overall Coverage</h2>
        <div class="metric">
            <div class="metric-value">${overall.statements}%</div>
            <div>Statements ${getStatusIcon(overall.statements)}</div>
        </div>
        <div class="metric">
            <div class="metric-value">${overall.branches}%</div>
            <div>Branches ${getStatusIcon(overall.branches)}</div>
        </div>
        <div class="metric">
            <div class="metric-value">${overall.functions}%</div>
            <div>Functions ${getStatusIcon(overall.functions)}</div>
        </div>
        <div class="metric">
            <div class="metric-value">${overall.lines}%</div>
            <div>Lines ${getStatusIcon(overall.lines)}</div>
        </div>
    </div>

    <div class="files">
        <h2>File Coverage</h2>
        <table>
            <tr>
                <th>File</th>
                <th>Statements</th>
                <th>Branches</th>
                <th>Functions</th>
                <th>Lines</th>
            </tr>`;

    Object.entries(files).forEach(([file, metrics]) => {
      html += `
            <tr>
                <td>${file}</td>
                <td class="${metrics.statements >= 85 ? 'pass' : 'fail'}">${metrics.statements}%</td>
                <td class="${metrics.branches >= 85 ? 'pass' : 'fail'}">${metrics.branches}%</td>
                <td class="${metrics.functions >= 85 ? 'pass' : 'fail'}">${metrics.functions}%</td>
                <td class="${metrics.lines >= 85 ? 'pass' : 'fail'}">${metrics.lines}%</td>
            </tr>`;
    });

    html += `
        </table>
    </div>
</body>
</html>`;

    return html;
  }

  /**
   * Generate Markdown coverage report
   */
  static generateMarkdownReport(coverageData, timestamp) {
    const { overall } = coverageData;
    const getStatusIcon = (value, threshold = 85) => value >= threshold ? '✅' : '❌';

    return `# Coverage Report

Generated: ${timestamp}

## Overall Coverage

| Metric | Percentage | Status |
|--------|------------|--------|
| Statements | ${overall.statements}% | ${getStatusIcon(overall.statements)} |
| Branches | ${overall.branches}% | ${getStatusIcon(overall.branches)} |
| Functions | ${overall.functions}% | ${getStatusIcon(overall.functions)} |
| Lines | ${overall.lines}% | ${getStatusIcon(overall.lines)} |

## Summary

${overall.statements >= 85 && overall.branches >= 85 && overall.functions >= 85 && overall.lines >= 85 
  ? '✅ **All coverage metrics meet the minimum 85% threshold**'
  : '❌ **Some coverage metrics are below the 85% threshold**'
}
`;
  }
}