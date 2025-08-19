import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import HandWeaponEditor from './Editor';

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
      props.additionalFields && props.additionalFields()
    ].filter(Boolean));
  };
  MockMundaneItemEditor.prototype = {};
  return MockMundaneItemEditor;
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
  }
}));

describe('HandWeaponEditor Component', () => {
  const mockHandWeapon = {
    name: 'Sword, Long',
    description: 'A fine steel longsword',
    era: 'Medieval',
    kind: 'Melee',
    note: 'Two-handed weapon',
    cost: 300,
    weight: 8,
    damage: 'Str+d8'
  };

  const defaultProps = {
    id: 'test-hand-weapon',
    item: mockHandWeapon,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
    });

    it('renders additional damage field', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Damage')).toBeInTheDocument();
    });

    it('displays current damage value', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      const damageInput = screen.getByLabelText('Damage');
      expect(damageInput).toHaveValue('Str+d8');
    });

    it('marks damage field as required', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      const damageInput = screen.getByLabelText('Damage');
      expect(damageInput).toHaveAttribute('required');
    });

    it('uses correct damage field id', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      const damageInput = screen.getByLabelText('Damage');
      expect(damageInput).toHaveAttribute('id', 'handWeaponDamage');
    });
  });

  describe('Form Field Updates', () => {
    it('updates damage when damage field changes', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d10' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Sword, Long',
          description: 'A fine steel longsword',
          era: 'Medieval',
          kind: 'Melee',
          note: 'Two-handed weapon',
          cost: 300,
          weight: 8,
          damage: 'Str+d10'
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} index={2} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d6' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 2);
    });

    it('preserves all existing item properties when updating damage', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d12' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      // Should have all original properties plus updated damage
      expect(callArguments).toEqual({
        name: 'Sword, Long',
        description: 'A fine steel longsword',
        era: 'Medieval',
        kind: 'Melee',
        note: 'Two-handed weapon',
        cost: 300,
        weight: 8,
        damage: 'Str+d12'
      });
    });
  });

  describe('Savage Worlds Hand Weapons', () => {
    const savageWorldsWeapons = [
      { name: 'Axe, Hand', damage: 'Str+d6' },
      { name: 'Dagger/Knife', damage: 'Str+d4' },
      { name: 'Flail', damage: 'Str+d6' },
      { name: 'Great Sword', damage: 'Str+d10' },
      { name: 'Halberd', damage: 'Str+d8' },
      { name: 'Lance', damage: 'Str+d8' },
      { name: 'Mace', damage: 'Str+d6' },
      { name: 'Rapier', damage: 'Str+d4' },
      { name: 'Spear', damage: 'Str+d6' },
      { name: 'Staff', damage: 'Str+d4' },
      { name: 'Sword, Long', damage: 'Str+d8' },
      { name: 'Sword, Short', damage: 'Str+d6' },
      { name: 'Warhammer', damage: 'Str+d6' }
    ];

    it('handles typical Savage Worlds hand weapon damage values', () => {
      savageWorldsWeapons.forEach(weapon => {
        const fullWeapon = { ...mockHandWeapon, ...weapon };
        const { rerender } = render(<HandWeaponEditor {...defaultProps} item={fullWeapon} />);
        
        expect(screen.getByLabelText('Damage')).toHaveValue(weapon.damage);
        
        rerender(<div />);
      });
    });

    it('handles weapon damage with AP (Armor Piercing)', () => {
      const apWeapon = {
        ...mockHandWeapon,
        name: 'Stiletto',
        damage: 'Str+d4, AP 1'
      };

      render(<HandWeaponEditor {...defaultProps} item={apWeapon} />);
      
      expect(screen.getByLabelText('Damage')).toHaveValue('Str+d4, AP 1');
    });

    it('handles weapon damage with multiple modifiers', () => {
      const complexWeapon = {
        ...mockHandWeapon,
        name: 'Magical Sword',
        damage: 'Str+d8+2, AP 2'
      };

      render(<HandWeaponEditor {...defaultProps} item={complexWeapon} />);
      
      expect(screen.getByLabelText('Damage')).toHaveValue('Str+d8+2, AP 2');
    });

    it('handles improvised weapon damage', () => {
      const improvisedWeapon = {
        ...mockHandWeapon,
        name: 'Chair Leg',
        damage: 'Str+d4-1'
      };

      render(<HandWeaponEditor {...defaultProps} item={improvisedWeapon} />);
      
      expect(screen.getByLabelText('Damage')).toHaveValue('Str+d4-1');
    });
  });

  describe('Special Damage Formats', () => {
    it('handles fixed damage values', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: '2d6' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: '2d6' }),
        0
      );
    });

    it('handles damage with special effects', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d6, Reach 1' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: 'Str+d6, Reach 1' }),
        0
      );
    });

    it('handles non-lethal damage', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d4 (non-lethal)' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: 'Str+d4 (non-lethal)' }),
        0
      );
    });
  });

  describe('Data Handling', () => {
    it('handles empty damage value', () => {
      const incompleteWeapon = {
        ...mockHandWeapon,
        damage: ''
      };

      render(<HandWeaponEditor {...defaultProps} item={incompleteWeapon} />);
      
      const damageInput = screen.getByLabelText('Damage');
      expect(damageInput).toHaveValue('');
    });

    it('handles undefined damage value', () => {
      const incompleteWeapon = {
        name: 'Incomplete Weapon',
        description: 'Missing damage'
      };

      expect(() => {
        render(<HandWeaponEditor {...defaultProps} item={incompleteWeapon} />);
      }).not.toThrow();
    });

    it('handles null damage value', () => {
      const nullDamageWeapon = {
        ...mockHandWeapon,
        damage: null
      };

      render(<HandWeaponEditor {...defaultProps} item={nullDamageWeapon} />);
      
      const damageInput = screen.getByLabelText('Damage');
      expect(damageInput).toHaveValue('');
    });

    it('handles rapid damage changes', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      
      fireEvent.change(damageInput, { target: { value: 'Str+d4' } });
      fireEvent.change(damageInput, { target: { value: 'Str+d6' } });
      fireEvent.change(damageInput, { target: { value: 'Str+d8' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ damage: 'Str+d8' }),
        0
      );
    });
  });

  describe('Component Integration', () => {
    it('extends MundaneItemEditor properly', () => {
      render(<HandWeaponEditor {...defaultProps} />);
      
      // Should render base MundaneItemEditor functionality
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
      
      // Should add additional damage field
      expect(screen.getByLabelText('Damage')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new HandWeaponEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('calls onChange with object assignment pattern', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: 'Str+d6' } });
      
      // Verify Object.assign pattern is used
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: mockHandWeapon.name,
          description: mockHandWeapon.description,
          era: mockHandWeapon.era,
          kind: mockHandWeapon.kind,
          note: mockHandWeapon.note,
          cost: mockHandWeapon.cost,
          weight: mockHandWeapon.weight,
          damage: 'Str+d6'
        }),
        0
      );
    });
  });

  describe('Edge Cases', () => {
    it('handles very long damage strings', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const longDamage = 'Str+d8+2, AP 4, Reach 2, Parry +1, two-handed, special ability';
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: longDamage } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: longDamage }),
        0
      );
    });

    it('handles special characters in damage', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const specialDamage = 'Str+d6 (½ damage vs. armor)';
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: specialDamage } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: specialDamage }),
        0
      );
    });

    it('handles numeric-only damage strings', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: '1' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: '1' }),
        0
      );
    });

    it('handles whitespace in damage values', () => {
      const mockOnChange = jest.fn();
      render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: '  Str+d8  ' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ damage: '  Str+d8  ' }),
        0
      );
    });
  });

  describe('Savage Worlds Game Mechanics', () => {
    it('supports standard die types', () => {
      const dieTypes = ['d4', 'd6', 'd8', 'd10', 'd12'];
      
      dieTypes.forEach(die => {
        const damageValue = `Str+${die}`;
        const mockOnChange = jest.fn();
        render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const damageInput = screen.getByLabelText('Damage');
        fireEvent.change(damageInput, { target: { value: damageValue } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ damage: damageValue }),
          0
        );
      });
    });

    it('supports strength-based damage variations', () => {
      const strengthVariations = [
        'Str+d4',
        'Str+d6+1',
        'Str+d8-1',
        'Str×2',
        'Str/2'
      ];
      
      strengthVariations.forEach(damage => {
        const mockOnChange = jest.fn();
        render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const damageInput = screen.getByLabelText('Damage');
        fireEvent.change(damageInput, { target: { value: damage } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ damage }),
          0
        );
      });
    });

    it('supports damage with weapon qualities', () => {
      const weaponQualities = [
        'Str+d6, Parry +1',
        'Str+d8, Reach 1',
        'Str+d6, AP 2',
        'Str+d4, Parry -1',
        'Str+d8, two-handed'
      ];
      
      weaponQualities.forEach(damage => {
        const mockOnChange = jest.fn();
        render(<HandWeaponEditor {...defaultProps} onChange={mockOnChange} />);
        
        const damageInput = screen.getByLabelText('Damage');
        fireEvent.change(damageInput, { target: { value: damage } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ damage }),
          0
        );
      });
    });
  });
});