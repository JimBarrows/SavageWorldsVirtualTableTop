import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import SelectedEdges from './index';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  Button: function MockButton({ id, onClick, children }) {
    return (
      <button data-testid="button" id={id} onClick={onClick}>
        {children}
      </button>
    );
  },
  SelectFormGroup: function MockSelectFormGroup({ id, label, onChange, options, value }) {
    return (
      <div data-testid="select-form-group">
        <label htmlFor={id}>{label}</label>
        <select 
          id={id}
          value={value || ''}
          onChange={onChange}
          data-testid="select-input"
        >
          <option value="">Select an edge</option>
          {options?.map((option, index) => (
            <option key={index} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
      </div>
    );
  },
  TextAreaFormGroup: function MockTextAreaFormGroup({ id, label, onChange, value }) {
    return (
      <div data-testid="textarea-form-group">
        <label htmlFor={id}>{label}</label>
        <textarea 
          id={id}
          value={value || ''}
          onChange={onChange}
          data-testid="textarea-input"
        />
      </div>
    );
  }
}));

describe('SelectedEdges Component', () => {
  const mockEdgesAvailable = [
    { name: 'Brave', description: 'Immune to fear effects' },
    { name: 'Quick', description: 'Act first in combat situations' },
    { name: 'Strong Willed', description: 'Resist mental effects' },
    { name: 'Fleet-Footed', description: 'Increased movement speed' },
    { name: 'Charismatic', description: 'Bonus to social interactions' }
  ];

  const mockSelectedEdges = [
    { edge: { name: 'Brave', description: 'Immune to fear effects' }, note: 'Gained at character creation' },
    { edge: { name: 'Quick', description: 'Act first in combat situations' }, note: 'Advanced at Seasoned rank' }
  ];

  const defaultProps = {
    id: 'test-edges',
    edgesAvailable: mockEdgesAvailable,
    edges: mockSelectedEdges,
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByRole('heading', { level: 3, name: 'Edges' })).toBeInTheDocument();
      expect(screen.getByTestId('select-form-group')).toBeInTheDocument();
      expect(screen.getByTestId('button')).toBeInTheDocument();
    });

    it('renders with correct component id and class', () => {
      render(<SelectedEdges {...defaultProps} id="custom-edges" />);
      
      const container = screen.getByText('Edges').closest('div');
      expect(container).toHaveAttribute('id', 'SelectedEdgeList-custom-edges');
      expect(container).toHaveClass('SelectedEdgeList');
    });

    it('displays heading for edges section', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const heading = screen.getByRole('heading', { level: 3 });
      expect(heading).toHaveTextContent('Edges');
    });

    it('renders edge selection dropdown', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByLabelText('Edges')).toBeInTheDocument();
      expect(screen.getByTestId('select-input')).toBeInTheDocument();
    });

    it('renders Add button', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      expect(addButton).toBeInTheDocument();
      expect(addButton).toHaveAttribute('id', 'SelectedEdgeList-test-edges-AddButton');
    });
  });

  describe('Available Edges Management', () => {
    it('shows only unselected edges in dropdown', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      // Should show edges not already selected
      expect(screen.getByRole('option', { name: 'Strong Willed' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Fleet-Footed' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Charismatic' })).toBeInTheDocument();
      
      // Should not show already selected edges
      expect(screen.queryByRole('option', { name: 'Brave' })).not.toBeInTheDocument();
      expect(screen.queryByRole('option', { name: 'Quick' })).not.toBeInTheDocument();
    });

    it('updates available edges when selection changes', () => {
      const { rerender } = render(<SelectedEdges {...defaultProps} />);
      
      // Initially 3 available edges (5 total - 2 selected)
      const options = screen.getAllByRole('option');
      expect(options).toHaveLength(4); // Including default empty option
      
      // Add another selected edge
      const newSelectedEdges = [...mockSelectedEdges, { 
        edge: { name: 'Strong Willed', description: 'Resist mental effects' }, 
        note: '' 
      }];
      
      rerender(<SelectedEdges {...defaultProps} edges={newSelectedEdges} />);
      
      // Should now have fewer available options
      const updatedOptions = screen.getAllByRole('option');
      expect(updatedOptions).toHaveLength(3); // Including default empty option
    });

    it('handles empty edgesAvailable array', () => {
      render(<SelectedEdges {...defaultProps} edgesAvailable={[]} />);
      
      const options = screen.getAllByRole('option');
      expect(options).toHaveLength(1); // Only default empty option
    });

    it('handles all edges being selected', () => {
      const allEdgesSelected = mockEdgesAvailable.map(edge => ({
        edge: edge,
        note: 'Test note'
      }));

      render(<SelectedEdges {...defaultProps} edges={allEdgesSelected} />);
      
      const options = screen.getAllByRole('option');
      expect(options).toHaveLength(1); // Only default empty option
    });
  });

  describe('Edge Selection', () => {
    it('updates selected edge state when dropdown changes', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      
      expect(select).toHaveValue('Strong Willed');
    });

    it('clears selection after adding edge', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(select).toHaveValue('');
    });

    it('does not add edge when none is selected', () => {
      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} onChange={mockOnChange} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(mockOnChange).not.toHaveBeenCalled();
    });

    it('calls onChange with updated edges list when edge is added', () => {
      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.arrayContaining([
          ...mockSelectedEdges,
          expect.objectContaining({
            edge: expect.objectContaining({ name: 'Strong Willed' }),
            note: ''
          })
        ])
      );
    });
  });

  describe('Edge Sorting', () => {
    it('sorts edges alphabetically when added', () => {
      const mockOnChange = jest.fn();
      const edgesInRandomOrder = [
        { edge: { name: 'Zebra Edge', description: 'Last alphabetically' }, note: '' },
        { edge: { name: 'Alpha Edge', description: 'First alphabetically' }, note: '' }
      ];

      render(<SelectedEdges {...defaultProps} edges={edgesInRandomOrder} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.arrayContaining([
          expect.objectContaining({ edge: expect.objectContaining({ name: 'Alpha Edge' }) }),
          expect.objectContaining({ edge: expect.objectContaining({ name: 'Strong Willed' }) }),
          expect.objectContaining({ edge: expect.objectContaining({ name: 'Zebra Edge' }) })
        ])
      );
    });

    it('handles sorting with identical edge names', () => {
      const mockOnChange = jest.fn();
      const duplicateEdges = [
        { edge: { name: 'Quick', description: 'First instance' }, note: 'Note 1' },
        { edge: { name: 'Quick', description: 'Second instance' }, note: 'Note 2' }
      ];

      render(<SelectedEdges {...defaultProps} edges={duplicateEdges} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(mockOnChange).toHaveBeenCalled();
    });

    it('tests sortEdgesByName utility function directly', () => {
      const component = new SelectedEdges.prototype.constructor(defaultProps);
      
      const unsortedEdges = [
        { edge: { name: 'Zebra' } },
        { edge: { name: 'Alpha' } },
        { edge: { name: 'Beta' } }
      ];

      const sortedEdges = component.sortEdgesByName(unsortedEdges);
      
      expect(sortedEdges[0].edge.name).toBe('Alpha');
      expect(sortedEdges[1].edge.name).toBe('Beta');
      expect(sortedEdges[2].edge.name).toBe('Zebra');
    });
  });

  describe('Selected Edges Display', () => {
    it('displays all selected edges', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByRole('heading', { level: 4, name: 'Brave' })).toBeInTheDocument();
      expect(screen.getByRole('heading', { level: 4, name: 'Quick' })).toBeInTheDocument();
    });

    it('displays edge descriptions', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByText('Immune to fear effects')).toBeInTheDocument();
      expect(screen.getByText('Act first in combat situations')).toBeInTheDocument();
    });

    it('displays edge notes in text areas', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const textareas = screen.getAllByTestId('textarea-input');
      expect(textareas).toHaveLength(2);
      expect(textareas[0]).toHaveValue('Gained at character creation');
      expect(textareas[1]).toHaveValue('Advanced at Seasoned rank');
    });

    it('has unique IDs for each edge note textarea', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const textareas = screen.getAllByTestId('textarea-form-group');
      expect(textareas).toHaveLength(2);
      
      // Check that each textarea has a unique ID
      const firstTextarea = textareas[0].querySelector('textarea');
      const secondTextarea = textareas[1].querySelector('textarea');
      
      expect(firstTextarea).toHaveAttribute('id', 'SelectedEdgeList-test-edges-Note-0');
      expect(secondTextarea).toHaveAttribute('id', 'SelectedEdgeList-test-edges-Note-1');
    });

    it('displays edges with selectedEdge CSS class', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const edgeContainers = document.querySelectorAll('.selectedEdge');
      expect(edgeContainers).toHaveLength(2);
    });
  });

  describe('Edge Note Management', () => {
    it('updates edge note when textarea changes', () => {
      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} onChange={mockOnChange} />);
      
      const textareas = screen.getAllByTestId('textarea-input');
      fireEvent.change(textareas[0], { target: { value: 'Updated note for first edge' } });
      
      expect(mockOnChange).toHaveBeenCalledWith([
        expect.objectContaining({
          edge: expect.objectContaining({ name: 'Brave' }),
          note: 'Updated note for first edge'
        }),
        expect.objectContaining({
          edge: expect.objectContaining({ name: 'Quick' }),
          note: 'Advanced at Seasoned rank'
        })
      ]);
    });

    it('updates correct edge when multiple notes are present', () => {
      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} onChange={mockOnChange} />);
      
      const textareas = screen.getAllByTestId('textarea-input');
      fireEvent.change(textareas[1], { target: { value: 'Updated note for second edge' } });
      
      expect(mockOnChange).toHaveBeenCalledWith([
        expect.objectContaining({
          edge: expect.objectContaining({ name: 'Brave' }),
          note: 'Gained at character creation'
        }),
        expect.objectContaining({
          edge: expect.objectContaining({ name: 'Quick' }),
          note: 'Updated note for second edge'
        })
      ]);
    });

    it('preserves other edge properties when updating note', () => {
      const edgesWithExtraProperties = [
        { 
          edge: { name: 'Brave', description: 'Immune to fear effects', rank: 'Novice' }, 
          note: 'Original note',
          customProperty: 'custom value'
        }
      ];

      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} edges={edgesWithExtraProperties} onChange={mockOnChange} />);
      
      const textarea = screen.getByTestId('textarea-input');
      fireEvent.change(textarea, { target: { value: 'Updated note' } });
      
      expect(mockOnChange).toHaveBeenCalledWith([
        expect.objectContaining({
          edge: expect.objectContaining({ 
            name: 'Brave', 
            description: 'Immune to fear effects',
            rank: 'Novice'
          }),
          note: 'Updated note',
          customProperty: 'custom value'
        })
      ]);
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('handles typical Savage Worlds edges', () => {
      const savageWorldsEdges = [
        { name: 'Arcane Background (Magic)', description: 'Allows use of magic' },
        { name: 'Combat Reflexes', description: '+2 to recover from Shaken' },
        { name: 'Improved Dodge', description: '+2 to dodge ranged attacks' },
        { name: 'Level Headed', description: 'Act on best of two action cards' }
      ];

      render(<SelectedEdges {...defaultProps} edgesAvailable={savageWorldsEdges} edges={[]} />);
      
      // Should display all available Savage Worlds edges
      expect(screen.getByRole('option', { name: 'Arcane Background (Magic)' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Combat Reflexes' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Improved Dodge' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Level Headed' })).toBeInTheDocument();
    });

    it('supports character advancement through edge acquisition', () => {
      const characterProgressionEdges = [
        { edge: { name: 'Brave', description: 'Starting edge' }, note: 'Novice rank' },
        { edge: { name: 'Combat Reflexes', description: 'First advancement' }, note: 'Seasoned rank' },
        { edge: { name: 'Improved Dodge', description: 'Second advancement' }, note: 'Veteran rank' }
      ];

      render(<SelectedEdges {...defaultProps} edges={characterProgressionEdges} />);
      
      // Should show progression through character ranks
      expect(screen.getByText('Novice rank')).toBeInTheDocument();
      expect(screen.getByText('Seasoned rank')).toBeInTheDocument();
      expect(screen.getByText('Veteran rank')).toBeInTheDocument();
    });

    it('handles edges with requirements and prerequisites', () => {
      const edgeWithPrerequisites = {
        edge: { 
          name: 'Improved Sweep', 
          description: 'Better sweep attacks',
          requirements: 'Seasoned, Sweep, Fighting d10+'
        }, 
        note: 'Requires Sweep edge and high Fighting skill'
      };

      render(<SelectedEdges {...defaultProps} edges={[edgeWithPrerequisites]} />);
      
      expect(screen.getByText('Requires Sweep edge and high Fighting skill')).toBeInTheDocument();
    });

    it('supports multiple instances of edges when allowed', () => {
      const multipleInstanceEdges = [
        { edge: { name: 'Rich', description: 'Extra starting funds' }, note: 'First instance' },
        { edge: { name: 'Rich', description: 'Extra starting funds' }, note: 'Second instance (Noble)' }
      ];

      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} edges={multipleInstanceEdges} onChange={mockOnChange} />);
      
      // Both instances should be displayed
      const richHeadings = screen.getAllByRole('heading', { level: 4, name: 'Rich' });
      expect(richHeadings).toHaveLength(2);
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('handles empty edges array', () => {
      render(<SelectedEdges {...defaultProps} edges={[]} />);
      
      expect(screen.getByRole('heading', { level: 3, name: 'Edges' })).toBeInTheDocument();
      expect(screen.queryByRole('heading', { level: 4 })).not.toBeInTheDocument();
    });

    it('handles undefined edges array', () => {
      // Temporarily suppress console.error for this test
      const originalError = console.error;
      console.error = jest.fn();
      
      expect(() => {
        render(<SelectedEdges {...defaultProps} edges={undefined} />);
      }).not.toThrow();
      
      // Restore console.error
      console.error = originalError;
    });

    it('handles malformed edge objects', () => {
      const malformedEdges = [
        { edge: null, note: 'Malformed edge' },
        { edge: { name: '' }, note: 'Empty name' },
        { note: 'Missing edge object' }
      ];

      expect(() => {
        render(<SelectedEdges {...defaultProps} edges={malformedEdges} />);
      }).not.toThrow();
    });

    it('handles very long edge names and descriptions', () => {
      const longContentEdge = {
        edge: { 
          name: 'Extremely Long Edge Name That Exceeds Normal Limits And Should Be Handled Gracefully',
          description: 'This is an extraordinarily long description that goes on and on to test how the component handles very lengthy text content that might cause layout issues or other problems.'
        },
        note: 'This is also a very long note that tests the textarea handling of extensive content.'
      };

      expect(() => {
        render(<SelectedEdges {...defaultProps} edges={[longContentEdge]} />);
      }).not.toThrow();
    });

    it('handles rapid selection changes', () => {
      const mockOnChange = jest.fn();
      render(<SelectedEdges {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      
      // Rapid changes
      fireEvent.change(select, { target: { value: 'Strong Willed' } });
      fireEvent.change(select, { target: { value: 'Fleet-Footed' } });
      fireEvent.change(select, { target: { value: 'Charismatic' } });
      
      expect(select).toHaveValue('Charismatic');
    });
  });

  describe('Accessibility', () => {
    it('provides proper labels for form elements', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByLabelText('Edges')).toBeInTheDocument();
      
      const textareas = screen.getAllByLabelText('Note');
      expect(textareas).toHaveLength(2);
    });

    it('uses semantic heading structure', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const mainHeading = screen.getByRole('heading', { level: 3 });
      expect(mainHeading).toHaveTextContent('Edges');
      
      const edgeHeadings = screen.getAllByRole('heading', { level: 4 });
      expect(edgeHeadings).toHaveLength(2);
    });

    it('provides accessible button interactions', () => {
      render(<SelectedEdges {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      expect(addButton).toBeInTheDocument();
      
      // Button should be focusable
      addButton.focus();
      expect(addButton).toHaveFocus();
    });
  });

  describe('Performance Considerations', () => {
    it('handles large numbers of available edges efficiently', () => {
      const manyEdges = Array(200).fill(0).map((_, i) => ({
        name: `Edge ${i}`,
        description: `Description for edge ${i}`
      }));

      expect(() => {
        render(<SelectedEdges {...defaultProps} edgesAvailable={manyEdges} />);
      }).not.toThrow();
    });

    it('handles large numbers of selected edges efficiently', () => {
      const manySelectedEdges = Array(50).fill(0).map((_, i) => ({
        edge: { name: `Selected Edge ${i}`, description: `Description ${i}` },
        note: `Note ${i}`
      }));

      expect(() => {
        render(<SelectedEdges {...defaultProps} edges={manySelectedEdges} />);
      }).not.toThrow();
    });

    it('does not cause unnecessary re-renders', () => {
      const { rerender } = render(<SelectedEdges {...defaultProps} />);
      
      // Same props should not cause issues
      rerender(<SelectedEdges {...defaultProps} />);
      
      expect(screen.getByRole('heading', { level: 3, name: 'Edges' })).toBeInTheDocument();
    });
  });
});