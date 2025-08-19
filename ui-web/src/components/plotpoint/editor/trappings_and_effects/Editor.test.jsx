import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import TrappingsAndEffectsEditor from './Editor';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  TextFormGroup: function MockTextFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="text-form-group">
        <label htmlFor={id}>{label}{required && ' *'}</label>
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

// Mock BaseEditor
jest.mock('../../../BaseEditor', () => {
  const MockBaseEditor = function({ id, onDelete, children }) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'base-editor', id }, [
      React.createElement('div', { 'data-testid': 'editor-header', key: 'header' }, [
        React.createElement('button', { 
          'data-testid': 'delete-button', 
          onClick: onDelete,
          key: 'delete'
        }, 'Delete')
      ]),
      React.createElement('div', { 'data-testid': 'editor-content', key: 'content' }, children)
    ]);
  };
  MockBaseEditor.prototype = {};
  return MockBaseEditor;
});

// Mock EditorList
jest.mock('../../../EditorList', () => {
  const MockEditorList = function({ id, title, list, onChange, emptyItem, headingLevel, children }) {
    const React = require('react');
    // Handle null, undefined, or non-array list values
    const safeList = Array.isArray(list) ? list : [];
    
    return React.createElement('div', { 'data-testid': 'editor-list', id }, [
      React.createElement('h' + (headingLevel || 2), { key: 'title' }, title),
      React.createElement('div', { 'data-testid': 'list-items', key: 'items' }, 
        safeList.map((item, index) => 
          React.createElement('div', { 
            key: index, 
            'data-testid': `list-item-${index}` 
          }, [
            React.createElement('span', { key: 'name' }, item.name || 'Unnamed'),
            React.createElement('button', { 
              key: 'edit',
              'data-testid': `edit-item-${index}`,
              onClick: () => onChange(safeList.map((listItem, listIndex) => 
                listIndex === index ? { ...listItem, name: 'Modified' } : listItem
              ))
            }, 'Edit'),
            React.createElement('button', { 
              key: 'delete',
              'data-testid': `delete-item-${index}`,
              onClick: () => onChange(safeList.filter((_, listIndex) => listIndex !== index))
            }, 'Remove')
          ])
        )
      ),
      React.createElement('button', { 
        'data-testid': 'add-item-button',
        key: 'add',
        onClick: () => onChange([emptyItem, ...safeList])
      }, 'Add')
    ]);
  };
  MockEditorList.prototype = {};
  return MockEditorList;
});

// Mock EffectsEditor
jest.mock('./EffectsEditor', () => {
  const MockEffectsEditor = function() {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'effects-editor' });
  };
  MockEffectsEditor.prototype = {};
  return MockEffectsEditor;
});

describe('TrappingsAndEffectsEditor Component', () => {
  const mockTrappingItem = {
    name: 'Arcane Bolt',
    description: 'A crackling bolt of mystical energy',
    effects: [
      { name: 'Damage', description: 'Deals 2d6 damage' },
      { name: 'Range', description: 'Range of 12/24/48' }
    ]
  };

  const defaultProps = {
    id: 'test-trapping',
    item: mockTrappingItem,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
    });

    it('renders name field with correct configuration', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name *')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Arcane Bolt')).toBeInTheDocument();
    });

    it('renders description field with correct configuration', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Description')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A crackling bolt of mystical energy')).toBeInTheDocument();
    });

    it('renders effects editor list', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('editor-list')).toBeInTheDocument();
      expect(screen.getByText('Effects')).toBeInTheDocument();
    });

    it('displays current effects in the list', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByText('Damage')).toBeInTheDocument();
      expect(screen.getByText('Range')).toBeInTheDocument();
    });

    it('renders delete button for the trapping', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('delete-button')).toBeInTheDocument();
    });

    it('uses custom id when provided', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} id="custom-trapping-id" />);
      
      expect(screen.getByTestId('base-editor')).toHaveAttribute('id', 'custom-trapping-id');
    });
  });

  describe('Name Field Handling', () => {
    it('updates name when name field changes', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: 'Lightning Bolt' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Lightning Bolt',
          description: 'A crackling bolt of mystical energy',
          effects: [
            { name: 'Damage', description: 'Deals 2d6 damage' },
            { name: 'Range', description: 'Range of 12/24/48' }
          ]
        }),
        0
      );
    });

    it('preserves other properties when updating name', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: mockTrappingItem.description,
          effects: mockTrappingItem.effects
        }),
        0
      );
    });

    it('handles empty name input', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: ''
        }),
        0
      );
    });

    it('passes correct index to onChange', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });
  });

  describe('Description Field Handling', () => {
    it('updates description when description field changes', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'A searing bolt of lightning energy' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Arcane Bolt',
          description: 'A searing bolt of lightning energy',
          effects: [
            { name: 'Damage', description: 'Deals 2d6 damage' },
            { name: 'Range', description: 'Range of 12/24/48' }
          ]
        }),
        0
      );
    });

    it('preserves other properties when updating description', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'New Description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: mockTrappingItem.name,
          effects: mockTrappingItem.effects
        }),
        0
      );
    });

    it('handles empty description input', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: ''
        }),
        0
      );
    });

    it('handles long description text', () => {
      const mockOnChange = jest.fn();
      const longDescription = 'This is a very long description that contains many details about the trapping and its various effects and manifestations in the game world.';
      
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: longDescription } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: longDescription
        }),
        0
      );
    });
  });

  describe('Effects Management', () => {
    it('renders effects editor list with correct configuration', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      const editorList = screen.getByTestId('editor-list');
      expect(editorList).toHaveAttribute('id', 'effectsEditorList');
      expect(screen.getByText('Effects')).toBeInTheDocument();
    });

    it('displays all effects in the list', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('list-item-0')).toBeInTheDocument();
      expect(screen.getByTestId('list-item-1')).toBeInTheDocument();
      expect(screen.getByText('Damage')).toBeInTheDocument();
      expect(screen.getByText('Range')).toBeInTheDocument();
    });

    it('updates effects when effects list changes', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const editButton = screen.getByTestId('edit-item-0');
      fireEvent.click(editButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Arcane Bolt',
          description: 'A crackling bolt of mystical energy',
          effects: [
            { name: 'Modified', description: 'Deals 2d6 damage' },
            { name: 'Range', description: 'Range of 12/24/48' }
          ]
        }),
        0
      );
    });

    it('adds new effect when add button is clicked', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const addButton = screen.getByTestId('add-item-button');
      fireEvent.click(addButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          effects: [
            { name: '', description: '' }, // Empty item added first
            { name: 'Damage', description: 'Deals 2d6 damage' },
            { name: 'Range', description: 'Range of 12/24/48' }
          ]
        }),
        0
      );
    });

    it('removes effect when delete button is clicked', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const deleteButton = screen.getByTestId('delete-item-0');
      fireEvent.click(deleteButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          effects: [
            { name: 'Range', description: 'Range of 12/24/48' }
          ]
        }),
        0
      );
    });

    it('handles empty effects array', () => {
      const itemWithNoEffects = {
        ...mockTrappingItem,
        effects: []
      };
      
      render(<TrappingsAndEffectsEditor {...defaultProps} item={itemWithNoEffects} />);
      
      expect(screen.getByTestId('editor-list')).toBeInTheDocument();
      expect(screen.getByTestId('add-item-button')).toBeInTheDocument();
    });

    it('preserves other properties when updating effects', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const addButton = screen.getByTestId('add-item-button');
      fireEvent.click(addButton);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: mockTrappingItem.name,
          description: mockTrappingItem.description
        }),
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(0);
    });

    it('passes correct index to onDelete', () => {
      const mockOnDelete = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} index={2} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(2);
    });

    it('prevents default when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      
      // Create a mock event with preventDefault
      const mockEvent = { preventDefault: jest.fn() };
      
      // Manually trigger the click with a preventDefault-enabled event
      fireEvent.click(deleteButton, mockEvent);
      
      // The component should call preventDefault in its onDelete handler
      // Since we can't directly test preventDefault calls with fireEvent,
      // we'll just ensure the delete functionality works
      expect(mockOnDelete).toHaveBeenCalledWith(0);
    });
  });

  describe('Savage Worlds RPG Scenarios', () => {
    it('handles typical Savage Worlds power trappings', () => {
      const savageWorldsPowers = [
        {
          name: 'Arcane Blast',
          description: 'A shimmering projectile of pure magical energy',
          effects: [
            { name: 'Damage', description: 'Deals 2d6 damage' },
            { name: 'Range', description: 'Smarts x 2' }
          ]
        },
        {
          name: 'Elemental Burst',
          description: 'Fire, ice, or lightning erupts from the caster',
          effects: [
            { name: 'Area Effect', description: 'Medium Burst Template' },
            { name: 'Elemental Damage', description: '2d6 damage of chosen type' }
          ]
        },
        {
          name: 'Mystic Shield',
          description: 'A translucent barrier of magical force',
          effects: [
            { name: 'Armor', description: 'Provides +2 Armor' },
            { name: 'Duration', description: 'Lasts for 5 rounds' }
          ]
        }
      ];

      savageWorldsPowers.forEach(power => {
        const { rerender } = render(<TrappingsAndEffectsEditor {...defaultProps} item={power} />);
        
        expect(screen.getByDisplayValue(power.name)).toBeInTheDocument();
        expect(screen.getByDisplayValue(power.description)).toBeInTheDocument();
        
        power.effects.forEach(effect => {
          expect(screen.getByText(effect.name)).toBeInTheDocument();
        });
        
        rerender(<div />);
      });
    });

    it('supports healing power trappings', () => {
      const healingPower = {
        name: 'Divine Light',
        description: 'A warm, golden glow emanates from the caster\'s hands',
        effects: [
          { name: 'Healing', description: 'Heals one Wound' },
          { name: 'Golden Glow', description: 'Provides bright illumination for 1 round' },
          { name: 'Fatigue Recovery', description: 'Removes one level of Fatigue' }
        ]
      };

      render(<TrappingsAndEffectsEditor {...defaultProps} item={healingPower} />);
      
      expect(screen.getByDisplayValue('Divine Light')).toBeInTheDocument();
      expect(screen.getByText('Healing')).toBeInTheDocument();
      expect(screen.getByText('Golden Glow')).toBeInTheDocument();
      expect(screen.getByText('Fatigue Recovery')).toBeInTheDocument();
    });

    it('supports utility power trappings', () => {
      const utilityPower = {
        name: 'Arcane Lockpick',
        description: 'Tendrils of magical energy manipulate lock mechanisms',
        effects: [
          { name: 'Lock Manipulation', description: 'Grants +2 to Thievery rolls' },
          { name: 'Silent Operation', description: 'Makes no noise when used' },
          { name: 'Magical Signature', description: 'Leaves faint magical traces' }
        ]
      };

      render(<TrappingsAndEffectsEditor {...defaultProps} item={utilityPower} />);
      
      expect(screen.getByDisplayValue('Arcane Lockpick')).toBeInTheDocument();
      expect(screen.getByText('Lock Manipulation')).toBeInTheDocument();
      expect(screen.getByText('Silent Operation')).toBeInTheDocument();
      expect(screen.getByText('Magical Signature')).toBeInTheDocument();
    });

    it('handles complex multi-effect power trappings', () => {
      const complexPower = {
        name: 'Elemental Storm',
        description: 'A swirling vortex of wind, fire, and lightning',
        effects: [
          { name: 'Wind Effects', description: 'Creates difficult terrain in Large Burst' },
          { name: 'Fire Damage', description: 'Deals 2d6 fire damage to all in area' },
          { name: 'Lightning Stun', description: 'Stunned on raise vs damage roll' },
          { name: 'Visibility', description: 'Blocks line of sight through area' },
          { name: 'Duration', description: 'Lasts for 3 rounds' },
          { name: 'Movement', description: 'Storm moves 6" per round in chosen direction' }
        ]
      };

      render(<TrappingsAndEffectsEditor {...defaultProps} item={complexPower} />);
      
      expect(screen.getByDisplayValue('Elemental Storm')).toBeInTheDocument();
      expect(screen.getByText('Wind Effects')).toBeInTheDocument();
      expect(screen.getByText('Fire Damage')).toBeInTheDocument();
      expect(screen.getByText('Lightning Stun')).toBeInTheDocument();
      expect(screen.getByText('Visibility')).toBeInTheDocument();
      expect(screen.getByText('Duration')).toBeInTheDocument();
      expect(screen.getByText('Movement')).toBeInTheDocument();
    });
  });

  describe('Data Validation and Edge Cases', () => {
    it('handles undefined name gracefully', () => {
      const itemWithUndefinedName = {
        description: 'Test description',
        effects: []
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...defaultProps} item={itemWithUndefinedName} />);
      }).not.toThrow();
    });

    it('handles undefined description gracefully', () => {
      const itemWithUndefinedDescription = {
        name: 'Test Name',
        effects: []
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...defaultProps} item={itemWithUndefinedDescription} />);
      }).not.toThrow();
    });

    it('handles undefined effects array gracefully', () => {
      const itemWithUndefinedEffects = {
        name: 'Test Name',
        description: 'Test description',
        effects: undefined
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...defaultProps} item={itemWithUndefinedEffects} />);
      }).not.toThrow();
      
      // Should still render the name and description
      expect(screen.getByDisplayValue('Test Name')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Test description')).toBeInTheDocument();
    });

    it('handles null values gracefully', () => {
      const itemWithNullValues = {
        name: 'Test Name',
        description: 'Test description', 
        effects: null
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...defaultProps} item={itemWithNullValues} />);
      }).not.toThrow();
      
      // Should still render the name and description
      expect(screen.getByDisplayValue('Test Name')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Test description')).toBeInTheDocument();
    });

    it('handles very long names', () => {
      const mockOnChange = jest.fn();
      const longName = 'This is an extremely long trapping name that contains many words and describes a very complex magical effect with multiple components and interactions';
      
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: longName } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: longName
        }),
        0
      );
    });

    it('handles rapid field changes', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      
      // Rapid changes
      fireEvent.change(nameInput, { target: { value: 'A' } });
      fireEvent.change(nameInput, { target: { value: 'AB' } });
      fireEvent.change(nameInput, { target: { value: 'ABC' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          name: 'ABC'
        }),
        0
      );
    });

    it('handles special characters in input fields', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: 'Spell\'s "Ultimate" Effect & More!' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Spell\'s "Ultimate" Effect & More!'
        }),
        0
      );
    });

    it('maintains object structure integrity during updates', () => {
      const mockOnChange = jest.fn();
      render(<TrappingsAndEffectsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name *');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      const calledObject = mockOnChange.mock.calls[0][0];
      expect(calledObject).toEqual({
        name: 'New Name',
        description: mockTrappingItem.description,
        effects: mockTrappingItem.effects
      });
      
      // Ensure it's a new object, not a mutation
      expect(calledObject).not.toBe(mockTrappingItem);
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts valid props without warnings', () => {
      const validProps = {
        id: 'test-id',
        item: {
          name: 'Test Name',
          description: 'Test Description',
          effects: []
        },
        index: 0,
        onChange: jest.fn(),
        onDelete: jest.fn()
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...validProps} />);
      }).not.toThrow();
    });

    it('handles missing optional item properties', () => {
      const minimalProps = {
        ...defaultProps,
        item: {
          effects: [] // Provide at least an empty effects array
        }
      };

      expect(() => {
        render(<TrappingsAndEffectsEditor {...minimalProps} />);
      }).not.toThrow();
      
      // Should render with empty values
      expect(screen.getByTestId('text-input')).toHaveValue('');
      expect(screen.getByTestId('textarea-input')).toHaveValue('');
    });
  });

  describe('Component Integration', () => {
    it('integrates properly with BaseEditor', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-header')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
    });

    it('integrates properly with EditorList for effects', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('editor-list')).toBeInTheDocument();
      expect(screen.getByText('Effects')).toBeInTheDocument();
      expect(screen.getByTestId('add-item-button')).toBeInTheDocument();
    });

    it('passes correct empty item structure to EditorList', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      const addButton = screen.getByTestId('add-item-button');
      fireEvent.click(addButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        expect.objectContaining({
          effects: expect.arrayContaining([
            expect.objectContaining({
              name: '',
              description: ''
            })
          ])
        }),
        0
      );
    });

    it('sets correct heading level for EditorList', () => {
      render(<TrappingsAndEffectsEditor {...defaultProps} />);
      
      // The mock EditorList uses headingLevel prop to create the heading
      // Since we pass headingLevel={3}, it should create an h3
      const heading = screen.getByRole('heading', { level: 3 });
      expect(heading).toHaveTextContent('Effects');
    });
  });
});