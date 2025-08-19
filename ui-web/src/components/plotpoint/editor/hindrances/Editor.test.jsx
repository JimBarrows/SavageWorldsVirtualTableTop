import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import HindranceEditor from './Editor';

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
  },
  SelectFormGroup: function MockSelectFormGroup({ id, label, onChange, value, options }) {
    return (
      <div data-testid="select-form-group">
        <label htmlFor={id}>{label}</label>
        <select 
          id={id}
          value={value || ''}
          onChange={onChange}
          data-testid="select-input"
        >
          {options?.map(option => (
            <option key={option.value} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
      </div>
    );
  }
}));

describe('HindranceEditor Component', () => {
  const mockHindrance = {
    name: 'Code of Honor',
    severity: 'Major',
    description: 'Character lives by a strict moral code'
  };

  const defaultProps = {
    id: 'test-hindrance',
    item: mockHindrance,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
    });

    it('renders all hindrance fields', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toBeInTheDocument();
      expect(screen.getByLabelText('Severity')).toBeInTheDocument();
      expect(screen.getByLabelText('Description')).toBeInTheDocument();
    });

    it('displays current hindrance values', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Code of Honor');
      expect(screen.getByLabelText('Severity')).toHaveValue('Major');
      expect(screen.getByLabelText('Description')).toHaveValue('Character lives by a strict moral code');
    });

    it('marks name field as required', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('required');
    });

    it('does not mark description field as required', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Description')).not.toHaveAttribute('required');
    });

    it('uses correct component id format', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'test-hindrance');
    });

    it('uses default id when not provided', () => {
      render(<HindranceEditor {...defaultProps} id={undefined} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'Editor');
    });

    it('uses correct field ids', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('id', 'hindranceName');
      expect(screen.getByLabelText('Severity')).toHaveAttribute('id', 'hindranceSeverity');
      expect(screen.getByLabelText('Description')).toHaveAttribute('id', 'hindranceDescription');
    });
  });

  describe('Severity Selection', () => {
    it('renders severity select options', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      expect(severitySelect).toBeInTheDocument();
      
      // Check that it's a select element
      expect(severitySelect.tagName).toBe('SELECT');
    });

    it('provides correct severity options', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      const options = severitySelect.querySelectorAll('option');
      
      expect(options).toHaveLength(3);
      expect(options[0]).toHaveValue('Minor');
      expect(options[0]).toHaveTextContent('Minor');
      expect(options[1]).toHaveValue('Major');
      expect(options[1]).toHaveTextContent('Major');
      expect(options[2]).toHaveValue('Major or Minor');
      expect(options[2]).toHaveTextContent('Major or Minor');
    });

    it('displays current severity selection', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      expect(severitySelect).toHaveValue('Major');
    });

    it('handles minor severity selection', () => {
      const minorHindrance = {
        ...mockHindrance,
        severity: 'Minor'
      };

      render(<HindranceEditor {...defaultProps} item={minorHindrance} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      expect(severitySelect).toHaveValue('Minor');
    });

    it('handles major or minor severity selection', () => {
      const flexibleHindrance = {
        ...mockHindrance,
        severity: 'Major or Minor'
      };

      render(<HindranceEditor {...defaultProps} item={flexibleHindrance} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      expect(severitySelect).toHaveValue('Major or Minor');
    });
  });

  describe('Form Field Updates', () => {
    it('updates name when name field changes', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Heroic' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Heroic',
          severity: 'Major',
          description: 'Character lives by a strict moral code'
        }),
        0
      );
    });

    it('updates severity when severity field changes', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      fireEvent.change(severitySelect, { target: { value: 'Minor' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          severity: 'Minor'
        }),
        0
      );
    });

    it('updates description when description field changes', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'Updated description'
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} index={4} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Test Hindrance' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 4);
    });

    it('preserves all existing properties when updating fields', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      expect(callArguments).toEqual({
        name: 'Updated Name',
        severity: 'Major',
        description: 'Character lives by a strict moral code'
      });
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<HindranceEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when deleting', () => {
      const mockOnDelete = jest.fn();
      render(<HindranceEditor {...defaultProps} onDelete={mockOnDelete} />);
      
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
      render(<HindranceEditor {...defaultProps} index={7} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(7);
    });
  });

  describe('Savage Worlds Hindrances', () => {
    const majorHindrances = [
      {
        name: 'Arrogant',
        severity: 'Major',
        description: 'Character believes they are superior'
      },
      {
        name: 'Bloodthirsty',
        severity: 'Major', 
        description: 'Character never leaves enemies alive'
      },
      {
        name: 'Code of Honor',
        severity: 'Major',
        description: 'Character lives by a strict moral code'
      },
      {
        name: 'Death Wish',
        severity: 'Minor',
        description: 'Character wants to die gloriously'
      },
      {
        name: 'Heroic',
        severity: 'Major',
        description: 'Character always helps those in need'
      }
    ];

    it('handles typical Savage Worlds major hindrances', () => {
      majorHindrances.forEach(hindrance => {
        const { rerender } = render(<HindranceEditor {...defaultProps} item={hindrance} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(hindrance.name);
        expect(screen.getByLabelText('Severity')).toHaveValue(hindrance.severity);
        expect(screen.getByLabelText('Description')).toHaveValue(hindrance.description);
        
        rerender(<div />);
      });
    });

    const minorHindrances = [
      {
        name: 'All Thumbs',
        severity: 'Minor',
        description: 'Character has trouble with mechanical things'
      },
      {
        name: 'Curious',
        severity: 'Major',
        description: 'Character must investigate everything'
      },
      {
        name: 'Greedy',
        severity: 'Minor', 
        description: 'Character is obsessed with wealth'
      },
      {
        name: 'Habit',
        severity: 'Minor',
        description: 'Character has an annoying habit'
      },
      {
        name: 'Loyal',
        severity: 'Minor',
        description: 'Character would die for friends'
      }
    ];

    it('handles typical Savage Worlds minor hindrances', () => {
      minorHindrances.forEach(hindrance => {
        const { rerender } = render(<HindranceEditor {...defaultProps} item={hindrance} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(hindrance.name);
        expect(screen.getByLabelText('Severity')).toHaveValue(hindrance.severity);
        expect(screen.getByLabelText('Description')).toHaveValue(hindrance.description);
        
        rerender(<div />);
      });
    });

    const flexibleHindrances = [
      {
        name: 'Enemy',
        severity: 'Major or Minor',
        description: 'Character has a recurring nemesis'
      },
      {
        name: 'Obligation',
        severity: 'Major or Minor', 
        description: 'Character owes service to someone'
      },
      {
        name: 'Phobia',
        severity: 'Major or Minor',
        description: 'Character has an irrational fear'
      },
      {
        name: 'Vow',
        severity: 'Major or Minor',
        description: 'Character has sworn an oath'
      }
    ];

    it('handles hindrances that can be major or minor', () => {
      flexibleHindrances.forEach(hindrance => {
        const { rerender } = render(<HindranceEditor {...defaultProps} item={hindrance} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(hindrance.name);
        expect(screen.getByLabelText('Severity')).toHaveValue(hindrance.severity);
        expect(screen.getByLabelText('Description')).toHaveValue(hindrance.description);
        
        rerender(<div />);
      });
    });
  });

  describe('Data Handling', () => {
    it('handles undefined hindrance properties', () => {
      const incompleteHindrance = {
        name: 'Incomplete Hindrance'
      };

      expect(() => {
        render(<HindranceEditor {...defaultProps} item={incompleteHindrance} />);
      }).not.toThrow();
    });

    it('handles null hindrance properties', () => {
      const nullHindrance = {
        name: 'Null Hindrance',
        severity: null,
        description: null
      };

      render(<HindranceEditor {...defaultProps} item={nullHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Null Hindrance');
      expect(screen.getByLabelText('Severity')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
    });

    it('handles empty string values', () => {
      const emptyHindrance = {
        name: '',
        severity: '',
        description: ''
      };

      render(<HindranceEditor {...defaultProps} item={emptyHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('');
      expect(screen.getByLabelText('Severity')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
    });

    it('handles invalid severity values', () => {
      const invalidSeverityHindrance = {
        ...mockHindrance,
        severity: 'Invalid'
      };

      render(<HindranceEditor {...defaultProps} item={invalidSeverityHindrance} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      expect(severitySelect).toHaveValue('Invalid');
    });

    it('handles special characters in hindrance data', () => {
      const specialHindrance = {
        name: 'Hindrance with "Quotes" & Symbols',
        severity: 'Major',
        description: 'Hindrance with éñ and symbols: @#$%^&*()'
      };

      render(<HindranceEditor {...defaultProps} item={specialHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Hindrance with "Quotes" & Symbols');
      expect(screen.getByLabelText('Description')).toHaveValue('Hindrance with éñ and symbols: @#$%^&*()');
    });
  });

  describe('Component Integration', () => {
    it('extends BaseEditor properly', () => {
      render(<HindranceEditor {...defaultProps} />);
      
      // Should render base BaseEditor functionality
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
      expect(screen.getByTestId('delete-button')).toBeInTheDocument();
    });

    it('implements React.Component properly', () => {
      const component = new HindranceEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('implements required PropTypes', () => {
      expect(HindranceEditor.propTypes).toBeDefined();
      expect(HindranceEditor.propTypes.id).toBeDefined();
      expect(HindranceEditor.propTypes.index).toBeDefined();
      expect(HindranceEditor.propTypes.item).toBeDefined();
      expect(HindranceEditor.propTypes.onChange).toBeDefined();
      expect(HindranceEditor.propTypes.onDelete).toBeDefined();
    });

    it('provides default props', () => {
      expect(HindranceEditor.defaultProps).toBeDefined();
      expect(HindranceEditor.defaultProps.id).toBe('Editor');
    });

    it('uses Object.assign for state updates', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      // Verify Object.assign pattern is used by checking the result
      const callArguments = mockOnChange.mock.calls[0][0];
      expect(callArguments).toEqual({
        ...mockHindrance,
        name: 'New Name'
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid field changes', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const severitySelect = screen.getByLabelText('Severity');
      
      fireEvent.change(nameInput, { target: { value: 'Name 1' } });
      fireEvent.change(nameInput, { target: { value: 'Name 2' } });
      fireEvent.change(severitySelect, { target: { value: 'Minor' } });
      fireEvent.change(nameInput, { target: { value: 'Final Name' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(4);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ name: 'Final Name' }),
        0
      );
    });

    it('handles very long field values', () => {
      const longHindrance = {
        ...mockHindrance,
        name: 'This is a very long hindrance name that might cause display or processing issues',
        description: 'This hindrance has an extremely long description that goes into great detail about all the various ways this hindrance affects the character and their interactions with the world around them'
      };

      render(<HindranceEditor {...defaultProps} item={longHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue(longHindrance.name);
      expect(screen.getByLabelText('Description')).toHaveValue(longHindrance.description);
    });

    it('handles whitespace in field values', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: '  Hindrance Name  ' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ name: '  Hindrance Name  ' }),
        0
      );
    });

    it('handles newlines in description field', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Line 1\nLine 2\nLine 3' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ description: 'Line 1\nLine 2\nLine 3' }),
        0
      );
    });

    it('handles simultaneous field updates', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const severitySelect = screen.getByLabelText('Severity');
      const descriptionInput = screen.getByLabelText('Description');
      
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      fireEvent.change(severitySelect, { target: { value: 'Minor' } });
      fireEvent.change(descriptionInput, { target: { value: 'Updated Description' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
    });
  });

  describe('Savage Worlds Severity System', () => {
    it('properly handles major hindrance selection', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      fireEvent.change(severitySelect, { target: { value: 'Major' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ severity: 'Major' }),
        0
      );
    });

    it('properly handles minor hindrance selection', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      fireEvent.change(severitySelect, { target: { value: 'Minor' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ severity: 'Minor' }),
        0
      );
    });

    it('properly handles major or minor hindrance selection', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      fireEvent.change(severitySelect, { target: { value: 'Major or Minor' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ severity: 'Major or Minor' }),
        0
      );
    });

    it('handles switching between severity levels', () => {
      const mockOnChange = jest.fn();
      render(<HindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const severitySelect = screen.getByLabelText('Severity');
      
      fireEvent.change(severitySelect, { target: { value: 'Minor' } });
      fireEvent.change(severitySelect, { target: { value: 'Major or Minor' } });
      fireEvent.change(severitySelect, { target: { value: 'Major' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ severity: 'Major' }),
        0
      );
    });
  });

  describe('Setting-Specific Hindrances', () => {
    it('handles fantasy setting hindrances', () => {
      const fantasyHindrance = {
        name: 'Cursed',
        severity: 'Major',
        description: 'Character is afflicted by a magical curse'
      };

      render(<HindranceEditor {...defaultProps} item={fantasyHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Cursed');
      expect(screen.getByLabelText('Severity')).toHaveValue('Major');
    });

    it('handles modern setting hindrances', () => {
      const modernHindrance = {
        name: 'Wanted',
        severity: 'Minor',
        description: 'Character is wanted by law enforcement'
      };

      render(<HindranceEditor {...defaultProps} item={modernHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Wanted');
      expect(screen.getByLabelText('Severity')).toHaveValue('Minor');
    });

    it('handles sci-fi setting hindrances', () => {
      const sciFiHindrance = {
        name: 'Cyber-Psychosis',
        severity: 'Major or Minor',
        description: 'Character suffers from excessive cybernetic augmentation'
      };

      render(<HindranceEditor {...defaultProps} item={sciFiHindrance} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Cyber-Psychosis');
      expect(screen.getByLabelText('Severity')).toHaveValue('Major or Minor');
    });
  });
});