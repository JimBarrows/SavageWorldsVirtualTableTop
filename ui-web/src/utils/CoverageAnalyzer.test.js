import CoverageAnalyzer from './CoverageAnalyzer';

describe('CoverageAnalyzer', () => {
  describe('parseCoverageReport', () => {
    it('should parse Jest coverage output correctly', () => {
      const jestOutput = `
=============================== Coverage summary ===============================
Statements   : 87.5% ( 175/200 )
Branches     : 85.2% ( 46/54 )
Functions    : 90.3% ( 28/31 )
Lines        : 88.1% ( 170/193 )
================================================================================
      `;
      
      const result = CoverageAnalyzer.parseCoverageReport(jestOutput, 'jest');
      
      expect(result.statements).toBe(87.5);
      expect(result.branches).toBe(85.2);
      expect(result.functions).toBe(90.3);
      expect(result.lines).toBe(88.1);
    });

    it('should parse LCOV file correctly', () => {
      const lcovContent = `SF:src/components/App.js
FNF:10
FNH:8
LF:50
LH:45
BRF:20
BRH:17
end_of_record
SF:src/utils/helper.js
FNF:5
FNH:4
LF:25
LH:20
BRF:10
BRH:8
end_of_record`;
      
      const result = CoverageAnalyzer.parseCoverageReport(lcovContent, 'lcov');
      
      expect(result.functions).toBe(80); // (8+4)/(10+5) = 12/15 = 80%
      expect(result.lines).toBe(86.67); // (45+20)/(50+25) = 65/75 = 86.67%
      expect(result.branches).toBe(83.33); // (17+8)/(20+10) = 25/30 = 83.33%
    });

    it('should handle empty or invalid input gracefully', () => {
      const result = CoverageAnalyzer.parseCoverageReport('', 'jest');
      
      expect(result.statements).toBe(0);
      expect(result.branches).toBe(0);
      expect(result.functions).toBe(0);
      expect(result.lines).toBe(0);
    });

    it('should handle unknown format by attempting Jest parsing', () => {
      const unknownFormat = `Some unknown format with coverage data`;
      
      const result = CoverageAnalyzer.parseCoverageReport(unknownFormat, 'unknown');
      
      expect(result).toBeDefined();
      expect(typeof result.statements).toBe('number');
    });
  });

  describe('analyzeCoverageFile', () => {
    it('should read and analyze coverage-final.json', async () => {
      const mockCoverageData = {
        'src/components/App.js': {
          s: { '0': 1, '1': 0, '2': 1, '3': 1 },
          b: { '0': [1, 0], '1': [1, 1] },
          f: { '0': 1, '1': 0 },
          statementMap: { '0': {}, '1': {}, '2': {}, '3': {} },
          branchMap: { '0': {}, '1': {} },
          fnMap: { '0': {}, '1': {} }
        }
      };
      
      // Mock fs.readFileSync
      const originalReadFileSync = require('fs').readFileSync;
      require('fs').readFileSync = jest.fn().mockReturnValue(JSON.stringify(mockCoverageData));
      
      const result = await CoverageAnalyzer.analyzeCoverageFile('coverage/coverage-final.json');
      
      expect(result.statements).toBe(75); // 3/4 statements covered
      expect(result.branches).toBe(75); // 3/4 branches covered
      expect(result.functions).toBe(50); // 1/2 functions covered
      
      // Restore original function
      require('fs').readFileSync = originalReadFileSync;
    });

    it('should handle file read errors gracefully', async () => {
      // Mock fs.readFileSync to throw error
      const originalReadFileSync = require('fs').readFileSync;
      require('fs').readFileSync = jest.fn().mockImplementation(() => {
        throw new Error('File not found');
      });
      
      const result = await CoverageAnalyzer.analyzeCoverageFile('nonexistent.json');
      
      expect(result.statements).toBe(0);
      expect(result.error).toBeDefined();
      
      // Restore original function
      require('fs').readFileSync = originalReadFileSync;
    });
  });

  describe('calculateOverallCoverage', () => {
    it('should calculate weighted coverage correctly', () => {
      const fileCoverageData = {
        'src/file1.js': {
          statements: { covered: 8, total: 10 },
          branches: { covered: 6, total: 8 },
          functions: { covered: 4, total: 5 },
          lines: { covered: 8, total: 10 }
        },
        'src/file2.js': {
          statements: { covered: 18, total: 20 },
          branches: { covered: 14, total: 16 },
          functions: { covered: 9, total: 10 },
          lines: { covered: 18, total: 20 }
        }
      };
      
      const result = CoverageAnalyzer.calculateOverallCoverage(fileCoverageData);
      
      expect(result.statements).toBe(86.67); // (8+18)/(10+20) = 26/30 = 86.67%
      expect(result.branches).toBe(83.33); // (6+14)/(8+16) = 20/24 = 83.33%
      expect(result.functions).toBe(86.67); // (4+9)/(5+10) = 13/15 = 86.67%
      expect(result.lines).toBe(86.67); // (8+18)/(10+20) = 26/30 = 86.67%
    });

    it('should handle empty coverage data', () => {
      const result = CoverageAnalyzer.calculateOverallCoverage({});
      
      expect(result.statements).toBe(0);
      expect(result.branches).toBe(0);
      expect(result.functions).toBe(0);
      expect(result.lines).toBe(0);
    });

    it('should handle division by zero gracefully', () => {
      const fileCoverageData = {
        'src/empty.js': {
          statements: { covered: 0, total: 0 },
          branches: { covered: 0, total: 0 },
          functions: { covered: 0, total: 0 },
          lines: { covered: 0, total: 0 }
        }
      };
      
      const result = CoverageAnalyzer.calculateOverallCoverage(fileCoverageData);
      
      expect(result.statements).toBe(0);
      expect(result.branches).toBe(0);
      expect(result.functions).toBe(0);
      expect(result.lines).toBe(0);
    });
  });

  describe('findUncoveredAreas', () => {
    it('should identify uncovered statements, branches, and functions', () => {
      const coverageData = {
        'src/example.js': {
          s: { '0': 1, '1': 0, '2': 1, '3': 0 },
          b: { '0': [1, 0], '1': [0, 0] },
          f: { '0': 1, '1': 0, '2': 1 },
          statementMap: {
            '1': { start: { line: 5, column: 2 }, end: { line: 5, column: 20 } },
            '3': { start: { line: 10, column: 4 }, end: { line: 10, column: 15 } }
          },
          branchMap: {
            '0': { 
              locations: [
                { start: { line: 7, column: 8 } },
                { start: { line: 7, column: 15 } }
              ]
            },
            '1': {
              locations: [
                { start: { line: 12, column: 6 } },
                { start: { line: 12, column: 20 } }
              ]
            }
          },
          fnMap: {
            '1': { name: 'uncoveredFunction', loc: { start: { line: 15, column: 0 } } }
          }
        }
      };
      
      const result = CoverageAnalyzer.findUncoveredAreas(coverageData);
      
      expect(result['src/example.js'].uncoveredStatements).toHaveLength(2);
      expect(result['src/example.js'].uncoveredBranches).toHaveLength(3); // 1 branch from b[0], 2 branches from b[1]
      expect(result['src/example.js'].uncoveredFunctions).toHaveLength(1);
      expect(result['src/example.js'].uncoveredFunctions[0].name).toBe('uncoveredFunction');
    });

    it('should handle files with complete coverage', () => {
      const coverageData = {
        'src/covered.js': {
          s: { '0': 1, '1': 1, '2': 1 },
          b: { '0': [1, 1], '1': [1, 1] },
          f: { '0': 1, '1': 1 },
          statementMap: {},
          branchMap: {},
          fnMap: {}
        }
      };
      
      const result = CoverageAnalyzer.findUncoveredAreas(coverageData);
      
      expect(result['src/covered.js'].uncoveredStatements).toHaveLength(0);
      expect(result['src/covered.js'].uncoveredBranches).toHaveLength(0);
      expect(result['src/covered.js'].uncoveredFunctions).toHaveLength(0);
    });
  });

  describe('generateCoverageReport', () => {
    it('should generate comprehensive HTML coverage report', () => {
      const coverageData = {
        overall: {
          statements: 87.5,
          branches: 85.0,
          functions: 90.0,
          lines: 88.0
        },
        files: {
          'src/App.js': {
            statements: 90,
            branches: 85,
            functions: 100,
            lines: 92
          },
          'src/utils.js': {
            statements: 80,
            branches: 75,
            functions: 85,
            lines: 82
          }
        }
      };
      
      const htmlReport = CoverageAnalyzer.generateCoverageReport(coverageData, 'html');
      
      expect(htmlReport).toContain('<html>');
      expect(htmlReport).toContain('Coverage Report');
      expect(htmlReport).toContain('87.5%'); // Overall statements
      expect(htmlReport).toContain('src/App.js');
      expect(htmlReport).toContain('src/utils.js');
    });

    it('should generate JSON coverage report', () => {
      const coverageData = {
        overall: {
          statements: 87.5,
          branches: 85.0,
          functions: 90.0,
          lines: 88.0
        }
      };
      
      const jsonReport = CoverageAnalyzer.generateCoverageReport(coverageData, 'json');
      const parsed = JSON.parse(jsonReport);
      
      expect(parsed.overall.statements).toBe(87.5);
      expect(parsed.timestamp).toBeDefined();
      expect(parsed.format).toBe('json');
    });

    it('should generate markdown coverage report', () => {
      const coverageData = {
        overall: {
          statements: 87.5,
          branches: 85.0,
          functions: 90.0,
          lines: 88.0
        }
      };
      
      const markdownReport = CoverageAnalyzer.generateCoverageReport(coverageData, 'markdown');
      
      expect(markdownReport).toContain('# Coverage Report');
      expect(markdownReport).toContain('| Statements');
      expect(markdownReport).toContain('87.5%');
      expect(markdownReport).toContain('✅'); // Should show passing status
    });
  });
});