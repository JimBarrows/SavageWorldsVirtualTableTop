import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Editor from './Editor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`number-form-${id}`}>
      <label>{label}</label>
      <input
        type="number"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`number-input-${id}`}
      />
    </div>
  ),
  TextAreaFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`textarea-form-${id}`}>
      <label>{label}</label>
      <textarea
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`textarea-${id}`}
      />
    </div>
  ),
  TextFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`text-form-${id}`}>
      <label>{label}</label>
      <input
        type="text"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`text-input-${id}`}
      />
    </div>
  )
}));

// Mock MundaneItemEditor parent class
jest.mock('../gear/mundane_items/Editor', () => {
  const mockReact = require('react');
  return class MockMundaneItemEditor extends mockReact.Component {
    static defaultProps = {
      id: 'MundaneItemEditorComponent'
    };

    costChange = e => this.props.onChange(Object.assign({}, this.props.item, {cost: parseInt(e.target.value, 10)}), this.props.index);
    weightChange = e => this.props.onChange(Object.assign({}, this.props.item, {weight: parseInt(e.target.value, 10)}), this.props.index);
    descriptionChange = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index);
    nameChange = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index);
    onKindChange = e => this.props.onChange(Object.assign({}, this.props.item, {kind: e.target.value}), this.props.index);
    onNoteChange = e => this.props.onChange(Object.assign({}, this.props.item, {note: e.target.value}), this.props.index);
    onEraChange = e => this.props.onChange(Object.assign({}, this.props.item, {era: e.target.value}), this.props.index);
    onDelete = event => {
      event.preventDefault();
      this.props.onDelete(this.props.index);
    };

    render() {
      let componentId = `GearEditor-${this.props.id}`;
      return (
        <div data-testid={`gear-editor-${this.props.id}`}>
          <button onClick={this.onDelete} data-testid={`delete-button-${this.props.id}`}>
            Delete
          </button>
          <div data-testid={`text-form-${componentId}-Name`}>
            <label>Name</label>
            <input
              type="text"
              onChange={this.nameChange}
              value={this.props.item.name || ''}
              required={true}
              data-testid={`text-input-${componentId}-Name`}
            />
          </div>
          <div data-testid={`textarea-form-${componentId}-Description`}>
            <label>Description</label>
            <textarea
              onChange={this.descriptionChange}
              value={this.props.item.description || ''}
              data-testid={`textarea-${componentId}-Description`}
            />
          </div>
          <div data-testid={`number-form-${this.props.id}-Cost`}>
            <label>Cost</label>
            <input
              type="number"
              onChange={this.costChange}
              value={this.props.item.cost || ''}
              required={true}
              data-testid={`number-input-${this.props.id}-Cost`}
            />
          </div>
          <div data-testid={`number-form-${this.props.id}-Weight`}>
            <label>Weight</label>
            <input
              type="number"
              onChange={this.weightChange}
              value={this.props.item.weight || ''}
              required={true}
              data-testid={`number-input-${this.props.id}-Weight`}
            />
          </div>
          <div data-testid={`text-form-${componentId}-Era`}>
            <label>Era</label>
            <input
              type="text"
              onChange={this.onEraChange}
              value={this.props.item.era || ''}
              required={true}
              data-testid={`text-input-${componentId}-Era`}
            />
          </div>
          <div data-testid={`text-form-${componentId}-Kind`}>
            <label>Kind</label>
            <input
              type="text"
              onChange={this.onKindChange}
              value={this.props.item.kind || ''}
              required={true}
              data-testid={`text-input-${componentId}-Kind`}
            />
          </div>
          <div data-testid={`textarea-form-${componentId}-Note`}>
            <label>Note</label>
            <textarea
              onChange={this.onNoteChange}
              value={this.props.item.note || ''}
              data-testid={`textarea-${componentId}-Note`}
            />
          </div>
          {this.additionalFields && this.additionalFields()}
        </div>
      );
    }
  };
});

describe('Armor Editor Component', () => {
  const defaultProps = {
    id: 'test-armor-editor',
    index: 0,
    item: {
      name: 'Chainmail',
      description: 'A suit of interlocking metal rings',
      cost: 300,
      weight: 25,
      era: 'Medieval',
      kind: 'Heavy Armor',
      note: 'Provides excellent protection against slashing attacks',
      armor: 3
    },
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<Editor {...defaultProps} />);
      expect(screen.getByTestId('gear-editor-test-armor-editor')).toBeInTheDocument();
    });

    it('renders inherited MundaneItemEditor fields', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Cost')).toBeInTheDocument();
      expect(screen.getByText('Weight')).toBeInTheDocument();
      expect(screen.getByText('Era')).toBeInTheDocument();
      expect(screen.getByText('Kind')).toBeInTheDocument();
      expect(screen.getByText('Note')).toBeInTheDocument();
    });

    it('renders additional armor field', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Armor')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-armorArmor')).toBeInTheDocument();
    });

    it('displays all field values correctly', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Chainmail')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A suit of interlocking metal rings')).toBeInTheDocument();
      expect(screen.getByDisplayValue('300')).toBeInTheDocument();
      expect(screen.getByDisplayValue('25')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Medieval')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Heavy Armor')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Provides excellent protection against slashing attacks')).toBeInTheDocument();
      expect(screen.getByDisplayValue('3')).toBeInTheDocument();
    });

    it('generates correct armor component ID', () => {
      render(<Editor {...defaultProps} />);
      
      const armorComponent = screen.getByTestId('ArmorEditorComponent-test-armor-editor');
      expect(armorComponent).toBeInTheDocument();
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      expect(armorInput).toBeInTheDocument();
    });
  });

  describe('Armor Field Updates', () => {
    it('updates armor value correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '5' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 5 },
        0
      );
    });

    it('handles zero armor value', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles empty string for armor as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles non-numeric input for armor as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: 'abc' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles floating point numbers by converting to integer', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '3.7' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 3 },
        0
      );
    });
  });

  describe('Inherited Field Updates', () => {
    it('updates name field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-GearEditor-test-armor-editor-Name');
      fireEvent.change(nameInput, { target: { value: 'Plate Armor' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Plate Armor' },
        0
      );
    });

    it('updates description field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-GearEditor-test-armor-editor-Description');
      fireEvent.change(descTextarea, { target: { value: 'Full body metal armor' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, description: 'Full body metal armor' },
        0
      );
    });

    it('updates cost field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const costInput = screen.getByTestId('number-input-test-armor-editor-Cost');
      fireEvent.change(costInput, { target: { value: '500' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, cost: 500 },
        0
      );
    });

    it('updates weight field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const weightInput = screen.getByTestId('number-input-test-armor-editor-Weight');
      fireEvent.change(weightInput, { target: { value: '40' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, weight: 40 },
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-armor-editor');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });
  });

  describe('Savage Worlds Armor Types', () => {
    const savageWorldsArmor = [
      {
        name: 'Leather Jacket',
        armor: 1,
        cost: 50,
        weight: 5,
        era: 'Modern',
        kind: 'Light Armor'
      },
      {
        name: 'Kevlar Vest',
        armor: 2,
        cost: 175,
        weight: 5,
        era: 'Modern',
        kind: 'Body Armor'
      },
      {
        name: 'Chainmail',
        armor: 3,
        cost: 300,
        weight: 25,
        era: 'Medieval',
        kind: 'Heavy Armor'
      },
      {
        name: 'Plate Mail',
        armor: 3,
        cost: 1500,
        weight: 50,
        era: 'Medieval',
        kind: 'Heavy Armor'
      },
      {
        name: 'Flak Jacket',
        armor: 2,
        cost: 200,
        weight: 8,
        era: 'Modern',
        kind: 'Military Armor'
      },
      {
        name: 'Riot Gear',
        armor: 3,
        cost: 500,
        weight: 15,
        era: 'Modern',
        kind: 'Police Armor'
      }
    ];

    it('handles standard Savage Worlds armor types', () => {
      savageWorldsArmor.forEach((armor, index) => {
        const props = { ...defaultProps, item: armor, id: `armor-${index}` };
        const { container } = render(<Editor {...props} />);
        
        expect(container.querySelector('input[value="' + armor.name + '"]')).toHaveValue(armor.name);
        expect(container.querySelector('input[value="' + armor.armor + '"]')).toHaveValue(armor.armor.toString());
        expect(container.querySelector('input[value="' + armor.cost + '"]')).toHaveValue(armor.cost.toString());
        expect(container.querySelector('input[value="' + armor.weight + '"]')).toHaveValue(armor.weight.toString());
      });
    });

    it('handles magical/fantasy armor', () => {
      const magicalArmor = [
        { name: 'Dragon Scale Mail', armor: 5, cost: 5000, weight: 30 },
        { name: 'Mithril Chainmail', armor: 4, cost: 10000, weight: 15 },
        { name: 'Enchanted Leather', armor: 2, cost: 800, weight: 8 }
      ];
      
      magicalArmor.forEach((armor, index) => {
        const props = { ...defaultProps, item: armor, id: `magical-${index}` };
        const { container } = render(<Editor {...props} />);
        
        const armorValueInput = container.querySelector('input[type="number"][value="' + armor.armor + '"]');
        expect(armorValueInput).toHaveValue(armor.armor);
      });
    });

    it('handles futuristic/sci-fi armor', () => {
      const sciFiArmor = [
        { name: 'Power Armor', armor: 6, cost: 50000, weight: 80 },
        { name: 'Energy Shield', armor: 4, cost: 25000, weight: 5 },
        { name: 'Nanofiber Suit', armor: 3, cost: 15000, weight: 2 }
      ];
      
      sciFiArmor.forEach((armor, index) => {
        const props = { ...defaultProps, item: armor, id: `scifi-${index}` };
        const { container } = render(<Editor {...props} />);
        
        const nameInput = container.querySelector('input[value="' + armor.name + '"]');
        expect(nameInput).toHaveValue(armor.name);
      });
    });
  });

  describe('Armor Value Validation', () => {
    it('handles typical armor values (0-10)', () => {
      for (let armorValue = 0; armorValue <= 10; armorValue++) {
        const props = { ...defaultProps, item: { ...defaultProps.item, armor: armorValue } };
        render(<Editor {...props} />);
        
        const armorInput = screen.getByTestId('number-input-armorArmor');
        expect(armorInput.value).toBe(armorValue.toString());
      }
    });

    it('handles very high armor values for magical/sci-fi settings', () => {
      const highArmorValues = [15, 20, 25, 50, 100];
      
      highArmorValues.forEach(value => {
        render(<Editor {...defaultProps} />);
        
        const armorInput = screen.getByTestId('number-input-armorArmor');
        fireEvent.change(armorInput, { target: { value: value.toString() } });
        
        expect(defaultProps.onChange).toHaveBeenCalledWith(
          { ...defaultProps.item, armor: value },
          0
        );
        
        jest.clearAllMocks();
      });
    });

    it('handles negative armor values (vulnerability)', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '-2' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: -2 },
        0
      );
    });
  });

  describe('Accessibility', () => {
    it('has armor field marked as required', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      expect(armorInput).toHaveAttribute('required');
    });

    it('has proper field type for armor', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      expect(armorInput).toHaveAttribute('type', 'number');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined armor property gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: { ...defaultProps.item, armor: undefined }
      };
      
      expect(() => render(<Editor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null armor property gracefully', () => {
      const propsWithNull = {
        ...defaultProps,
        item: { ...defaultProps.item, armor: null }
      };
      
      expect(() => render(<Editor {...propsWithNull} />)).not.toThrow();
    });

    it('handles string armor values', () => {
      const propsWithString = {
        ...defaultProps,
        item: { ...defaultProps.item, armor: '4' }
      };
      
      render(<Editor {...propsWithString} />);
      expect(screen.getByDisplayValue('4')).toBeInTheDocument();
    });

    it('handles different index values', () => {
      const props = { ...defaultProps, index: 7 };
      render(<Editor {...props} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '2' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 2 },
        7
      );
    });
  });

  describe('Component Integration', () => {
    it('inherits from MundaneItemEditor correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const gearEditor = screen.getByTestId('gear-editor-test-armor-editor');
      expect(gearEditor).toBeInTheDocument();
      
      // Check that inherited fields are present
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Cost')).toBeInTheDocument();
      expect(screen.getByText('Weight')).toBeInTheDocument();
    });

    it('extends functionality with armor-specific field', () => {
      render(<Editor {...defaultProps} />);
      
      // Check that the additional armor field is present
      const armorField = screen.getByTestId('number-form-armorArmor');
      expect(armorField).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-armorArmor');
      fireEvent.change(armorInput, { target: { value: '4' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, armor: 4 }
      };
      
      rerender(<Editor {...updatedProps} />);
      expect(screen.getByDisplayValue('4')).toBeInTheDocument();
    });

    it('handles multiple instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'armor-1', index: 0 };
      const props2 = { ...defaultProps, id: 'armor-2', index: 1 };
      
      const { container } = render(
        <div>
          <Editor {...props1} />
          <Editor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="gear-editor-"]')).toHaveLength(2);
    });
  });

  describe('Form Validation Scenarios', () => {
    it('handles form with all required fields filled', () => {
      const validProps = {
        ...defaultProps,
        item: {
          name: 'Valid Armor',
          cost: 200,
          weight: 15,
          era: 'Modern',
          kind: 'Body Armor',
          armor: 2
        }
      };
      
      render(<Editor {...validProps} />);
      
      expect(screen.getByDisplayValue('Valid Armor')).toBeInTheDocument();
      expect(screen.getByDisplayValue('200')).toBeInTheDocument();
      expect(screen.getByDisplayValue('15')).toBeInTheDocument();
      expect(screen.getByDisplayValue('2')).toBeInTheDocument();
    });

    it('handles form with empty required fields', () => {
      const emptyProps = {
        ...defaultProps,
        item: {
          name: '',
          cost: 0,
          weight: 0,
          era: '',
          kind: '',
          armor: 0
        }
      };
      
      render(<Editor {...emptyProps} />);
      
      const nameInput = screen.getByTestId('text-input-GearEditor-test-armor-editor-Name');
      const costInput = screen.getByTestId('number-input-test-armor-editor-Cost');
      const armorInput = screen.getByTestId('number-input-armorArmor');
      
      expect(nameInput).toHaveAttribute('required');
      expect(costInput).toHaveAttribute('required');
      expect(armorInput).toHaveAttribute('required');
    });
  });

  describe('Era and Kind Specific Tests', () => {
    const armorByEra = {
      'Stone Age': [
        { name: 'Hide Armor', armor: 1, cost: 25, weight: 8 }
      ],
      'Bronze Age': [
        { name: 'Bronze Scale Mail', armor: 2, cost: 150, weight: 20 }
      ],
      'Medieval': [
        { name: 'Chainmail', armor: 3, cost: 300, weight: 25 },
        { name: 'Plate Mail', armor: 3, cost: 1500, weight: 50 }
      ],
      'Renaissance': [
        { name: 'Breastplate', armor: 3, cost: 400, weight: 20 }
      ],
      'Modern': [
        { name: 'Kevlar Vest', armor: 2, cost: 175, weight: 5 },
        { name: 'Riot Gear', armor: 3, cost: 500, weight: 15 }
      ],
      'Future': [
        { name: 'Power Armor', armor: 6, cost: 50000, weight: 80 }
      ]
    };

    Object.entries(armorByEra).forEach(([era, armors]) => {
      it(`handles ${era} era armor correctly`, () => {
        armors.forEach((armor, index) => {
          const props = {
            ...defaultProps,
            item: { ...armor, era, kind: 'Armor' },
            id: `${era}-armor-${index}`
          };
          
          const { container } = render(<Editor {...props} />);
          
          const eraInput = container.querySelector('input[value="' + era + '"]');
          const armorValueInput = container.querySelector('input[type="number"][value="' + armor.armor + '"]');
          
          expect(eraInput).toHaveValue(era);
          expect(armorValueInput).toHaveValue(armor.armor);
        });
      });
    });
  });
});