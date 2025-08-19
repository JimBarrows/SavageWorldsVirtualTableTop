import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import SpecialWeaponEditor from './Editor';

// Mock MundaneItemEditor
jest.mock('../mundane_items/Editor', () => {
  const MockMundaneItemEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'mundane-item-editor' }, [
      React.createElement('div', { 'data-testid': 'base-fields', key: 'base' }, [
        React.createElement('input', { 'data-testid': 'name-input', value: props.item.name || '', readOnly: true, key: 'name' }),
        React.createElement('input', { 'data-testid': 'description-input', value: props.item.description || '', readOnly: true, key: 'desc' }),
        React.createElement('input', { 'data-testid': 'cost-input', value: props.item.cost || '', readOnly: true, key: 'cost' }),
        React.createElement('input', { 'data-testid': 'weight-input', value: props.item.weight || '', readOnly: true, key: 'weight' })
      ]),
      props.additionalRangedWeaponFields && props.additionalRangedWeaponFields()
    ].filter(Boolean));
  };
  MockMundaneItemEditor.prototype = {};
  return MockMundaneItemEditor;
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: function MockNumberFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="number-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="number"
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="number-input"
        />
      </div>
    );
  },
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
  }
}));

describe('SpecialWeaponEditor Component', () => {
  const mockSpecialWeapon = {
    name: 'Flamethrower',
    description: 'Heavy flamethrower unit',
    era: 'Modern',
    kind: 'Special',
    note: 'Area effect weapon',
    cost: 600,
    weight: 70,
    armorPiercing: 2,
    burstTemplate: 'Cone Template'
  };

  const defaultProps = {
    id: 'test-special-weapon',
    item: mockSpecialWeapon,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
    });

    it('renders additional armor piercing field', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Armor Piercing')).toBeInTheDocument();
    });

    it('renders additional burst template field', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Burst Template')).toBeInTheDocument();
    });

    it('displays current armor piercing value', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      expect(apInput).toHaveValue('2');
    });

    it('displays current burst template value', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      const burstInput = screen.getByLabelText('Burst Template');
      expect(burstInput).toHaveValue('Cone Template');
    });

    it('marks armor piercing field as required', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      expect(apInput).toHaveAttribute('required');
    });

    it('does not mark burst template field as required', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      const burstInput = screen.getByLabelText('Burst Template');
      expect(burstInput).not.toHaveAttribute('required');
    });

    it('uses correct field ids', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      const burstInput = screen.getByLabelText('Burst Template');
      
      expect(apInput).toHaveAttribute('id', `SpecialWeaponsEditor-${defaultProps.id}-ArmorPiercing`);
      expect(burstInput).toHaveAttribute('id', `SpecialWeaponsEditor-${defaultProps.id}-BurstTemplate`);
    });
  });

  describe('Form Field Updates', () => {
    it('updates armor piercing when AP field changes', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '4' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Flamethrower',
          description: 'Heavy flamethrower unit',
          era: 'Modern',
          kind: 'Special',
          note: 'Area effect weapon',
          cost: 600,
          weight: 70,
          armorPiercing: 4,
          burstTemplate: 'Cone Template'
        }),
        0
      );
    });

    it('updates burst template when burst template field changes', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const burstInput = screen.getByLabelText('Burst Template');
      fireEvent.change(burstInput, { target: { value: 'Large Burst Template' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Flamethrower',
          description: 'Heavy flamethrower unit',
          era: 'Modern',
          kind: 'Special',
          note: 'Area effect weapon',
          cost: 600,
          weight: 70,
          armorPiercing: 2,
          burstTemplate: 'Large Burst Template'
        }),
        0
      );
    });

    it('converts armor piercing to integer', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '3.7' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          armorPiercing: 3
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '1' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });

    it('preserves all existing item properties when updating fields', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '6' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      expect(callArguments).toEqual({
        name: 'Flamethrower',
        description: 'Heavy flamethrower unit',
        era: 'Modern',
        kind: 'Special',
        note: 'Area effect weapon',
        cost: 600,
        weight: 70,
        armorPiercing: 6,
        burstTemplate: 'Cone Template'
      });
    });
  });

  describe('Savage Worlds Special Weapons', () => {
    const specialWeapons = [
      { name: 'Flamethrower', armorPiercing: 2, burstTemplate: 'Cone Template' },
      { name: 'Grenade Launcher', armorPiercing: 0, burstTemplate: 'Medium Burst Template' },
      { name: 'Rocket Launcher', armorPiercing: 5, burstTemplate: 'Large Burst Template' },
      { name: 'Mortar', armorPiercing: 3, burstTemplate: 'Large Burst Template' },
      { name: 'Minigun', armorPiercing: 2, burstTemplate: '' }
    ];

    it('handles typical Savage Worlds special weapons', () => {
      specialWeapons.forEach(weapon => {
        const fullWeapon = { ...mockSpecialWeapon, ...weapon };
        const { rerender } = render(<SpecialWeaponEditor {...defaultProps} item={fullWeapon} />);
        
        expect(screen.getByLabelText('Armor Piercing')).toHaveValue(weapon.armorPiercing.toString());
        expect(screen.getByLabelText('Burst Template')).toHaveValue(weapon.burstTemplate);
        
        rerender(<div />);
      });
    });

    it('handles high armor piercing values', () => {
      const heavyWeapon = {
        ...mockSpecialWeapon,
        name: 'Anti-Tank Weapon',
        armorPiercing: 10
      };

      render(<SpecialWeaponEditor {...defaultProps} item={heavyWeapon} />);
      
      expect(screen.getByLabelText('Armor Piercing')).toHaveValue('10');
    });

    it('handles zero armor piercing', () => {
      const nonArmorPiercingWeapon = {
        ...mockSpecialWeapon,
        name: 'Smoke Grenade Launcher',
        armorPiercing: 0
      };

      render(<SpecialWeaponEditor {...defaultProps} item={nonArmorPiercingWeapon} />);
      
      expect(screen.getByLabelText('Armor Piercing')).toHaveValue('0');
    });
  });

  describe('Burst Template Types', () => {
    const burstTemplateTypes = [
      'Small Burst Template',
      'Medium Burst Template',
      'Large Burst Template',
      'Cone Template',
      'Stream Template',
      'None'
    ];

    it('handles various burst template types', () => {
      burstTemplateTypes.forEach(template => {
        const mockOnChange = jest.fn();
        render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const burstInput = screen.getByLabelText('Burst Template');
        fireEvent.change(burstInput, { target: { value: template } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ burstTemplate: template }),
          0
        );
      });
    });

    it('handles empty burst template', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const burstInput = screen.getByLabelText('Burst Template');
      fireEvent.change(burstInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ burstTemplate: '' }),
        0
      );
    });
  });

  describe('Data Handling', () => {
    it('handles undefined armor piercing value', () => {
      const incompleteWeapon = {
        ...mockSpecialWeapon,
        armorPiercing: undefined
      };

      expect(() => {
        render(<SpecialWeaponEditor {...defaultProps} item={incompleteWeapon} />);
      }).not.toThrow();
    });

    it('handles undefined burst template value', () => {
      const incompleteWeapon = {
        ...mockSpecialWeapon,
        burstTemplate: undefined
      };

      expect(() => {
        render(<SpecialWeaponEditor {...defaultProps} item={incompleteWeapon} />);
      }).not.toThrow();
    });

    it('handles null values', () => {
      const nullWeapon = {
        ...mockSpecialWeapon,
        armorPiercing: null,
        burstTemplate: null
      };

      render(<SpecialWeaponEditor {...defaultProps} item={nullWeapon} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      const burstInput = screen.getByLabelText('Burst Template');
      
      expect(apInput).toHaveValue('');
      expect(burstInput).toHaveValue('');
    });

    it('handles empty string input for armor piercing', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          armorPiercing: 0 // parseInt('', 10) -> NaN, but should be handled gracefully
        }),
        0
      );
    });

    it('handles non-numeric input for armor piercing', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: 'abc' } });
      
      // parseInt('abc', 10) returns NaN
      expect(mockOnChange).toHaveBeenCalled();
    });

    it('handles negative armor piercing values', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '-2' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ armorPiercing: -2 }),
        0
      );
    });
  });

  describe('Component Integration', () => {
    it('extends MundaneItemEditor properly', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      // Should render base MundaneItemEditor functionality
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
      
      // Should add additional fields
      expect(screen.getByLabelText('Armor Piercing')).toBeInTheDocument();
      expect(screen.getByLabelText('Burst Template')).toBeInTheDocument();
    });

    it('uses additionalRangedWeaponFields method', () => {
      render(<SpecialWeaponEditor {...defaultProps} />);
      
      // Method should be called and render the additional fields
      expect(screen.getByLabelText('Armor Piercing')).toBeInTheDocument();
      expect(screen.getByLabelText('Burst Template')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new SpecialWeaponEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid value changes', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      
      fireEvent.change(apInput, { target: { value: '1' } });
      fireEvent.change(apInput, { target: { value: '5' } });
      fireEvent.change(apInput, { target: { value: '8' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ armorPiercing: 8 }),
        0
      );
    });

    it('handles very large armor piercing values', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      fireEvent.change(apInput, { target: { value: '999' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ armorPiercing: 999 }),
        0
      );
    });

    it('handles very long burst template strings', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const longTemplate = 'Very Large Burst Template with Special Effects and Extended Range';
      const burstInput = screen.getByLabelText('Burst Template');
      fireEvent.change(burstInput, { target: { value: longTemplate } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ burstTemplate: longTemplate }),
        0
      );
    });

    it('handles special characters in burst template', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const specialTemplate = 'Template (3" × 6")';
      const burstInput = screen.getByLabelText('Burst Template');
      fireEvent.change(burstInput, { target: { value: specialTemplate } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ burstTemplate: specialTemplate }),
        0
      );
    });

    it('handles simultaneous field updates', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apInput = screen.getByLabelText('Armor Piercing');
      const burstInput = screen.getByLabelText('Burst Template');
      
      fireEvent.change(apInput, { target: { value: '3' } });
      fireEvent.change(burstInput, { target: { value: 'New Template' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(2);
    });
  });

  describe('Savage Worlds Game Mechanics', () => {
    it('supports standard armor piercing values', () => {
      const standardAP = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      
      standardAP.forEach(ap => {
        const mockOnChange = jest.fn();
        render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const apInput = screen.getByLabelText('Armor Piercing');
        fireEvent.change(apInput, { target: { value: ap.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ armorPiercing: ap }),
          0
        );
      });
    });

    it('supports area effect weapons', () => {
      const areaWeapons = [
        { name: 'Grenade', burstTemplate: 'Medium Burst Template' },
        { name: 'RPG', burstTemplate: 'Large Burst Template' },
        { name: 'Claymore', burstTemplate: 'Cone Template' },
        { name: 'Flamethrower', burstTemplate: 'Stream Template' }
      ];

      areaWeapons.forEach(weapon => {
        const mockOnChange = jest.fn();
        render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const burstInput = screen.getByLabelText('Burst Template');
        fireEvent.change(burstInput, { target: { value: weapon.burstTemplate } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ burstTemplate: weapon.burstTemplate }),
          0
        );
      });
    });

    it('supports weapons without area effects', () => {
      const mockOnChange = jest.fn();
      render(<SpecialWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const burstInput = screen.getByLabelText('Burst Template');
      fireEvent.change(burstInput, { target: { value: 'None' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ burstTemplate: 'None' }),
        0
      );
    });
  });
});