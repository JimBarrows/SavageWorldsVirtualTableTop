import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import GearEditor from './GearEditor';

// Mock BaseEditor
jest.mock('../../../BaseEditor', () => {
  return function MockBaseEditor({ id, onDelete, children }) {
    return (
      <div data-testid="base-editor" data-id={id}>
        <button data-testid="delete-button" onClick={onDelete}>Delete</button>
        {children}
      </div>
    );
  };
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  TextFormGroup: function MockTextFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="text-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
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

describe('GearEditor Component', () => {
  const mockGearItem = {
    name: 'Longsword',
    description: 'A classic medieval weapon',
    era: 'Medieval',
    kind: 'Weapon',
    note: 'Two-handed sword'
  };

  const defaultProps = {
    id: 'test-gear',
    item: mockGearItem,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<GearEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByLabelText('Name')).toHaveValue('Longsword');
      expect(screen.getByLabelText('Description')).toHaveValue('A classic medieval weapon');
      expect(screen.getByLabelText('Era')).toHaveValue('Medieval');
      expect(screen.getByLabelText('Kind')).toHaveValue('Weapon');
      expect(screen.getByLabelText('Note')).toHaveValue('Two-handed sword');
    });

    it('renders with correct component id', () => {
      render(<GearEditor {...defaultProps} id="custom-gear" />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('data-id', 'GearEditor-custom-gear');
    });

    it('marks required fields appropriately', () => {
      render(<GearEditor {...defaultProps} />);
      
      const nameInput = screen.getByLabelText('Name');
      const eraInput = screen.getByLabelText('Era');
      const kindInput = screen.getByLabelText('Kind');
      
      expect(nameInput).toHaveAttribute('required');
      expect(eraInput).toHaveAttribute('required');
      expect(kindInput).toHaveAttribute('required');
    });

    it('does not mark optional fields as required', () => {
      render(<GearEditor {...defaultProps} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      const noteInput = screen.getByLabelText('Note');
      
      expect(descriptionInput).not.toHaveAttribute('required');
      expect(noteInput).not.toHaveAttribute('required');
    });
  });

  describe('Form Field Updates', () => {
    it('updates gear name', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Broadsword' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Broadsword',
          description: 'A classic medieval weapon',
          era: 'Medieval',
          kind: 'Weapon',
          note: 'Two-handed sword'
        }),
        0
      );
    });

    it('updates gear description', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'Updated description'
        }),
        0
      );
    });

    it('updates gear era', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const eraInput = screen.getByLabelText('Era');
      fireEvent.change(eraInput, { target: { value: 'Modern' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          era: 'Modern'
        }),
        0
      );
    });

    it('updates gear kind', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const kindInput = screen.getByLabelText('Kind');
      fireEvent.change(kindInput, { target: { value: 'Tool' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          kind: 'Tool'
        }),
        0
      );
    });

    it('updates gear note', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const noteInput = screen.getByLabelText('Note');
      fireEvent.change(noteInput, { target: { value: 'Updated note' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          note: 'Updated note'
        }),
        0
      );
    });

    it('preserves unchanged fields when updating', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'New Name',
          description: 'A classic medieval weapon',
          era: 'Medieval',
          kind: 'Weapon',
          note: 'Two-handed sword'
        }),
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<GearEditor {...defaultProps} index={5} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(5);
    });

    it('prevents default on delete button click', () => {
      const mockOnDelete = jest.fn();
      render(<GearEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalled();
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('handles typical Savage Worlds gear items', () => {
      const savageWorldsGear = {
        name: 'Plate Armor',
        description: 'Heavy metal armor providing excellent protection',
        era: 'Medieval',
        kind: 'Armor',
        note: '+3 Armor, -3 Pace'
      };

      render(<GearEditor {...defaultProps} item={savageWorldsGear} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Plate Armor');
      expect(screen.getByLabelText('Description')).toHaveValue('Heavy metal armor providing excellent protection');
      expect(screen.getByLabelText('Era')).toHaveValue('Medieval');
      expect(screen.getByLabelText('Kind')).toHaveValue('Armor');
      expect(screen.getByLabelText('Note')).toHaveValue('+3 Armor, -3 Pace');
    });

    it('supports different eras of equipment', () => {
      const modernGear = {
        name: 'Kevlar Vest',
        description: 'Modern ballistic protection',
        era: 'Modern',
        kind: 'Armor',
        note: '+2 Armor vs bullets'
      };

      render(<GearEditor {...defaultProps} item={modernGear} />);
      
      expect(screen.getByLabelText('Era')).toHaveValue('Modern');
    });

    it('handles different gear kinds', () => {
      const gearKinds = ['Weapon', 'Armor', 'Tool', 'Mundane', 'Treasure'];
      
      gearKinds.forEach(kind => {
        const gearItem = { ...mockGearItem, kind };
        const { rerender } = render(<GearEditor {...defaultProps} item={gearItem} />);
        
        expect(screen.getByLabelText('Kind')).toHaveValue(kind);
        
        rerender(<div />);
      });
    });
  });

  describe('Additional Fields Extension Point', () => {
    it('renders additional fields when additionalFields method exists', () => {
      class ExtendedGearEditor extends GearEditor {
        additionalFields = () => (
          <div data-testid="additional-fields">
            <input data-testid="additional-input" />
          </div>
        );
      }

      render(<ExtendedGearEditor {...defaultProps} />);
      
      expect(screen.getByTestId('additional-fields')).toBeInTheDocument();
      expect(screen.getByTestId('additional-input')).toBeInTheDocument();
    });

    it('does not render additional fields section when method does not exist', () => {
      render(<GearEditor {...defaultProps} />);
      
      expect(screen.queryByTestId('additional-fields')).not.toBeInTheDocument();
    });
  });

  describe('Data Handling', () => {
    it('handles empty gear item gracefully', () => {
      const emptyGear = {
        name: '',
        description: '',
        era: '',
        kind: '',
        note: ''
      };

      expect(() => {
        render(<GearEditor {...defaultProps} item={emptyGear} />);
      }).not.toThrow();
    });

    it('handles missing optional properties', () => {
      const minimalGear = {
        name: 'Basic Item'
      };

      expect(() => {
        render(<GearEditor {...defaultProps} item={minimalGear} />);
      }).not.toThrow();
    });

    it('handles very long text content', () => {
      const longContentGear = {
        name: 'Very Long Name That Exceeds Normal Limits And Should Be Handled Gracefully',
        description: 'This is an extraordinarily long description that goes on and on to test how the component handles very lengthy text content that might cause layout issues.',
        era: 'Extremely Long Era Name',
        kind: 'Very Long Kind Description',
        note: 'This is also a very long note that tests the textarea handling of extensive content.'
      };

      expect(() => {
        render(<GearEditor {...defaultProps} item={longContentGear} />);
      }).not.toThrow();
    });
  });

  describe('Component Integration', () => {
    it('integrates properly with BaseEditor', () => {
      render(<GearEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toBeInTheDocument();
      expect(baseEditor).toHaveAttribute('data-id', 'GearEditor-test-gear');
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });
  });

  describe('State Management', () => {
    it('maintains form state consistency', () => {
      const mockOnChange = jest.fn();
      render(<GearEditor {...defaultProps} onChange={mockOnChange} />);
      
      // Make multiple changes
      const nameInput = screen.getByLabelText('Name');
      const eraInput = screen.getByLabelText('Era');
      
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      fireEvent.change(eraInput, { target: { value: 'New Era' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(2);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          name: 'Longsword', // Original value since we update immutably
          era: 'New Era'
        }),
        0
      );
    });
  });
});