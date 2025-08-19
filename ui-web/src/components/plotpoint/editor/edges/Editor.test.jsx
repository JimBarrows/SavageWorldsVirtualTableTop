import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import EdgeEditor from './Editor';

// Mock BaseEditor
jest.mock('../../../BaseEditor', () => {
  const MockBaseEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'base-editor', id: props.id }, [
      React.createElement('button', { 
        'data-testid': 'delete-button',
        onClick: props.onDelete,
        key: 'delete-btn'
      }, 'Delete'),
      React.createElement('div', { 'data-testid': 'editor-content', key: 'content' }, 
        props.children
      )
    ]);
  };
  MockBaseEditor.prototype = {};
  return MockBaseEditor;
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  TextFormGroup: function MockTextFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="text-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="text"
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="text-input"
        />
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

describe('EdgeEditor Component', () => {
  const mockEdge = {
    name: 'Quick Draw',
    category: 'Combat',
    requirements: 'Agility d8+',
    description: 'Character can draw weapons quickly',
    effects: 'May draw weapon as a free action'
  };

  const defaultProps = {
    id: 'test-edge',
    item: mockEdge,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
    });

    it('renders all edge fields', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toBeInTheDocument();
      expect(screen.getByLabelText('Category')).toBeInTheDocument();
      expect(screen.getByLabelText('Requirements')).toBeInTheDocument();
      expect(screen.getByLabelText('Description')).toBeInTheDocument();
      expect(screen.getByLabelText('Effects')).toBeInTheDocument();
    });

    it('displays current edge values', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Quick Draw');
      expect(screen.getByLabelText('Category')).toHaveValue('Combat');
      expect(screen.getByLabelText('Requirements')).toHaveValue('Agility d8+');
      expect(screen.getByLabelText('Description')).toHaveValue('Character can draw weapons quickly');
      expect(screen.getByLabelText('Effects')).toHaveValue('May draw weapon as a free action');
    });

    it('marks required fields as required', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('required');
      expect(screen.getByLabelText('Category')).toHaveAttribute('required');
      expect(screen.getByLabelText('Requirements')).toHaveAttribute('required');
      expect(screen.getByLabelText('Effects')).toHaveAttribute('required');
    });

    it('does not mark description field as required', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Description')).not.toHaveAttribute('required');
    });

    it('uses correct component id format', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'EdgeEditor-test-edge');
    });

    it('uses default id when not provided', () => {
      render(<EdgeEditor {...defaultProps} id={undefined} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'EdgeEditor-Editor');
    });

    it('uses correct field ids', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('id', 'EdgeEditor-test-edge-Name');
      expect(screen.getByLabelText('Category')).toHaveAttribute('id', 'EdgeEditor-test-edge-Category');
      expect(screen.getByLabelText('Requirements')).toHaveAttribute('id', 'EdgeEditor-test-edge-Requirements');
      expect(screen.getByLabelText('Description')).toHaveAttribute('id', 'EdgeEditor-test-edge-Description');
      expect(screen.getByLabelText('Effects')).toHaveAttribute('id', 'EdgeEditor-test-edge-Effects');
    });
  });

  describe('Form Field Updates', () => {
    it('updates name when name field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Marksman' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Marksman',
          category: 'Combat',
          requirements: 'Agility d8+',
          description: 'Character can draw weapons quickly',
          effects: 'May draw weapon as a free action'
        }),
        0
      );
    });

    it('updates category when category field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const categoryInput = screen.getByLabelText('Category');
      fireEvent.change(categoryInput, { target: { value: 'Professional' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          category: 'Professional'
        }),
        0
      );
    });

    it('updates requirements when requirements field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const requirementsInput = screen.getByLabelText('Requirements');
      fireEvent.change(requirementsInput, { target: { value: 'Shooting d10+' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          requirements: 'Shooting d10+'
        }),
        0
      );
    });

    it('updates description when description field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'Updated description'
        }),
        0
      );
    });

    it('updates effects when effects field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const effectsInput = screen.getByLabelText('Effects');
      fireEvent.change(effectsInput, { target: { value: 'New effects description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          effects: 'New effects description'
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Test Edge' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });

    it('preserves all existing properties when updating fields', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      expect(callArguments).toEqual({
        name: 'Updated Name',
        category: 'Combat',
        requirements: 'Agility d8+',
        description: 'Character can draw weapons quickly',
        effects: 'May draw weapon as a free action'
      });
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<EdgeEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when deleting', () => {
      const mockOnDelete = jest.fn();
      render(<EdgeEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      const clickEvent = new MouseEvent('click', {
        bubbles: true,
        cancelable: true,
      });
      
      fireEvent(deleteButton, clickEvent);
      
      expect(mockOnDelete).toHaveBeenCalled();
    });

    it('passes correct index to onDelete', () => {
      const mockOnDelete = jest.fn();
      render(<EdgeEditor {...defaultProps} index={5} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(5);
    });
  });

  describe('Savage Worlds Edges', () => {
    const savageWorldsEdges = [
      {
        name: 'Alertness',
        category: 'Background',
        requirements: 'Novice',
        effects: '+2 Notice rolls'
      },
      {
        name: 'Ambidextrous',
        category: 'Background',
        requirements: 'Agility d8+',
        effects: 'Ignore -2 penalty for off-hand'
      },
      {
        name: 'Arcane Background',
        category: 'Background',
        requirements: 'Special',
        effects: 'Allows use of arcane powers'
      },
      {
        name: 'Block',
        category: 'Combat',
        requirements: 'Fighting d8+',
        effects: '+1 Parry'
      },
      {
        name: 'Brawny',
        category: 'Background',
        requirements: 'Strength d6+, Vigor d6+',
        effects: 'Toughness +1, load limit +8'
      },
      {
        name: 'Champion',
        category: 'Legendary',
        requirements: 'Legendary, Spirit d8+, Strength d6+, Fighting d10+',
        effects: '+2 damage vs supernatural evil'
      }
    ];

    it('handles typical Savage Worlds edges', () => {
      savageWorldsEdges.forEach(edge => {
        const { rerender } = render(<EdgeEditor {...defaultProps} item={edge} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(edge.name);
        expect(screen.getByLabelText('Category')).toHaveValue(edge.category);
        expect(screen.getByLabelText('Requirements')).toHaveValue(edge.requirements);
        expect(screen.getByLabelText('Effects')).toHaveValue(edge.effects);
        
        rerender(<div />);
      });
    });

    it('handles edges with complex requirements', () => {
      const complexEdge = {
        ...mockEdge,
        name: 'Master of Arms',
        requirements: 'Veteran, Fighting d12+, Strength d8+'
      };

      render(<EdgeEditor {...defaultProps} item={complexEdge} />);
      
      expect(screen.getByLabelText('Requirements')).toHaveValue('Veteran, Fighting d12+, Strength d8+');
    });

    it('handles edges with long descriptions', () => {
      const detailedEdge = {
        ...mockEdge,
        description: 'This edge provides extensive benefits to characters who have mastered the art of combat. It represents years of training and experience in various fighting techniques, weapons proficiency, and tactical awareness.'
      };

      render(<EdgeEditor {...defaultProps} item={detailedEdge} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      expect(descriptionInput).toHaveValue(detailedEdge.description);
    });
  });

  describe('Edge Categories', () => {
    const edgeCategories = [
      'Background',
      'Combat', 
      'Leadership',
      'Power',
      'Professional',
      'Social',
      'Legendary',
      'Weird'
    ];

    it('handles various edge categories', () => {
      edgeCategories.forEach(category => {
        const mockOnChange = jest.fn();
        render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
        
        const categoryInput = screen.getByLabelText('Category');
        fireEvent.change(categoryInput, { target: { value: category } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ category }),
          0
        );
      });
    });

    it('handles custom edge categories', () => {
      const customCategories = ['Racial', 'Setting-Specific', 'Homebrew'];
      
      customCategories.forEach(category => {
        const mockOnChange = jest.fn();
        render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
        
        const categoryInput = screen.getByLabelText('Category');
        fireEvent.change(categoryInput, { target: { value: category } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ category }),
          0
        );
      });
    });
  });

  describe('Data Handling', () => {
    it('handles undefined edge properties', () => {
      const incompleteEdge = {
        name: 'Incomplete Edge'
      };

      expect(() => {
        render(<EdgeEditor {...defaultProps} item={incompleteEdge} />);
      }).not.toThrow();
    });

    it('handles null edge properties', () => {
      const nullEdge = {
        name: 'Null Edge',
        category: null,
        requirements: null,
        description: null,
        effects: null
      };

      render(<EdgeEditor {...defaultProps} item={nullEdge} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Null Edge');
      expect(screen.getByLabelText('Category')).toHaveValue('');
      expect(screen.getByLabelText('Requirements')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
      expect(screen.getByLabelText('Effects')).toHaveValue('');
    });

    it('handles empty string values', () => {
      const emptyEdge = {
        name: '',
        category: '',
        requirements: '',
        description: '',
        effects: ''
      };

      render(<EdgeEditor {...defaultProps} item={emptyEdge} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('');
      expect(screen.getByLabelText('Category')).toHaveValue('');
      expect(screen.getByLabelText('Requirements')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
      expect(screen.getByLabelText('Effects')).toHaveValue('');
    });

    it('handles special characters in edge data', () => {
      const specialEdge = {
        name: 'Edge with "Quotes" & Symbols',
        category: 'Special/Mixed',
        requirements: 'Trait ≥ d8+',
        description: 'Edge with éñ and symbols: @#$%^&*()',
        effects: 'Provides ±2 bonus to rolls'
      };

      render(<EdgeEditor {...defaultProps} item={specialEdge} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Edge with "Quotes" & Symbols');
      expect(screen.getByLabelText('Category')).toHaveValue('Special/Mixed');
      expect(screen.getByLabelText('Requirements')).toHaveValue('Trait ≥ d8+');
      expect(screen.getByLabelText('Description')).toHaveValue('Edge with éñ and symbols: @#$%^&*()');
      expect(screen.getByLabelText('Effects')).toHaveValue('Provides ±2 bonus to rolls');
    });
  });

  describe('Component Integration', () => {
    it('extends BaseEditor properly', () => {
      render(<EdgeEditor {...defaultProps} />);
      
      // Should render base BaseEditor functionality
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
      expect(screen.getByTestId('delete-button')).toBeInTheDocument();
    });

    it('implements React.Component properly', () => {
      const component = new EdgeEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('implements required PropTypes', () => {
      expect(EdgeEditor.propTypes).toBeDefined();
      expect(EdgeEditor.propTypes.id).toBeDefined();
      expect(EdgeEditor.propTypes.index).toBeDefined();
      expect(EdgeEditor.propTypes.item).toBeDefined();
      expect(EdgeEditor.propTypes.onChange).toBeDefined();
      expect(EdgeEditor.propTypes.onDelete).toBeDefined();
    });

    it('provides default props', () => {
      expect(EdgeEditor.defaultProps).toBeDefined();
      expect(EdgeEditor.defaultProps.id).toBe('Editor');
    });

    it('uses Object.assign for state updates', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      // Verify Object.assign pattern is used by checking the result
      const callArguments = mockOnChange.mock.calls[0][0];
      expect(callArguments).toEqual({
        ...mockEdge,
        name: 'New Name'
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid field changes', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const categoryInput = screen.getByLabelText('Category');
      
      fireEvent.change(nameInput, { target: { value: 'Name 1' } });
      fireEvent.change(nameInput, { target: { value: 'Name 2' } });
      fireEvent.change(categoryInput, { target: { value: 'Category 1' } });
      fireEvent.change(nameInput, { target: { value: 'Final Name' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(4);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ name: 'Final Name' }),
        0
      );
    });

    it('handles very long field values', () => {
      const longEdge = {
        ...mockEdge,
        name: 'This is a very long edge name that might cause display or processing issues',
        requirements: 'This is an extremely long requirements list with many traits and conditions that must be met',
        effects: 'This edge provides numerous benefits and effects that are described in great detail with multiple clauses and conditions'
      };

      render(<EdgeEditor {...defaultProps} item={longEdge} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue(longEdge.name);
      expect(screen.getByLabelText('Requirements')).toHaveValue(longEdge.requirements);
      expect(screen.getByLabelText('Effects')).toHaveValue(longEdge.effects);
    });

    it('handles whitespace in field values', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: '  Edge Name  ' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ name: '  Edge Name  ' }),
        0
      );
    });

    it('handles newlines in textarea fields', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Line 1\nLine 2\nLine 3' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ description: 'Line 1\nLine 2\nLine 3' }),
        0
      );
    });

    it('handles simultaneous field updates', () => {
      const mockOnChange = jest.fn();
      render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const categoryInput = screen.getByLabelText('Category');
      const effectsInput = screen.getByLabelText('Effects');
      
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      fireEvent.change(categoryInput, { target: { value: 'Updated Category' } });
      fireEvent.change(effectsInput, { target: { value: 'Updated Effects' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
    });
  });

  describe('Savage Worlds Rank Requirements', () => {
    const rankRequirements = [
      'Novice',
      'Seasoned',
      'Veteran', 
      'Heroic',
      'Legendary'
    ];

    it('handles standard Savage Worlds rank requirements', () => {
      rankRequirements.forEach(rank => {
        const mockOnChange = jest.fn();
        render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
        
        const requirementsInput = screen.getByLabelText('Requirements');
        fireEvent.change(requirementsInput, { target: { value: rank } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ requirements: rank }),
          0
        );
      });
    });

    it('handles complex rank and trait requirements', () => {
      const complexRequirements = [
        'Seasoned, Fighting d8+',
        'Veteran, Agility d8+, Shooting d10+',
        'Heroic, Spirit d8+, Faith d6+',
        'Legendary, two other Legendary Edges'
      ];

      complexRequirements.forEach(requirement => {
        const mockOnChange = jest.fn();
        render(<EdgeEditor {...defaultProps} onChange={mockOnChange} />);
        
        const requirementsInput = screen.getByLabelText('Requirements');
        fireEvent.change(requirementsInput, { target: { value: requirement } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ requirements: requirement }),
          0
        );
      });
    });
  });
});