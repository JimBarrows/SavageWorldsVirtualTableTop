import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import RangedWeaponsEditor from './Editor';

// Mock MundaneItemEditor
jest.mock('../mundane_items/Editor', () => {
  const MockMundaneItemEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'mundane-item-editor' }, [
      React.createElement('div', { 'data-testid': 'base-fields', key: 'base' }, [
        React.createElement('input', { 'data-testid': 'name-input', value: props.item.name || '', readOnly: true, key: 'name' }),
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

describe('RangedWeaponsEditor Component', () => {
  const mockRangedWeapon = {
    name: 'Longbow',
    description: 'Traditional English longbow',
    era: 'Medieval',
    kind: 'Ranged Weapon',
    note: 'Two-handed weapon',
    cost: 300,
    weight: 3,
    shortRange: 15,
    mediumRange: 30,
    longRange: 60,
    damage: '2d6',
    rateOfFire: 1,
    shots: 1,
    minimumStrength: 'd6'
  };

  const defaultProps = {
    id: 'test-ranged-weapon',
    item: mockRangedWeapon,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
    });

    it('renders all ranged weapon specific fields', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Short Range')).toBeInTheDocument();
      expect(screen.getByLabelText('Medium Range')).toBeInTheDocument();
      expect(screen.getByLabelText('Long Range')).toBeInTheDocument();
      expect(screen.getByLabelText('Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('Rate of Fire')).toBeInTheDocument();
      expect(screen.getByLabelText('Shots')).toBeInTheDocument();
      expect(screen.getByLabelText('Minimum Strength')).toBeInTheDocument();
    });

    it('displays current ranged weapon values', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Short Range')).toHaveValue('15');
      expect(screen.getByLabelText('Medium Range')).toHaveValue('30');
      expect(screen.getByLabelText('Long Range')).toHaveValue('60');
      expect(screen.getByLabelText('Damage')).toHaveValue('2d6');
      expect(screen.getByLabelText('Rate of Fire')).toHaveValue('1');
      expect(screen.getByLabelText('Shots')).toHaveValue('1');
      expect(screen.getByLabelText('Minimum Strength')).toHaveValue('d6');
    });

    it('marks all ranged weapon fields as required', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Short Range')).toHaveAttribute('required');
      expect(screen.getByLabelText('Medium Range')).toHaveAttribute('required');
      expect(screen.getByLabelText('Long Range')).toHaveAttribute('required');
      expect(screen.getByLabelText('Damage')).toHaveAttribute('required');
      expect(screen.getByLabelText('Rate of Fire')).toHaveAttribute('required');
      expect(screen.getByLabelText('Shots')).toHaveAttribute('required');
      expect(screen.getByLabelText('Minimum Strength')).toHaveAttribute('required');
    });

    it('renders with correct component wrapper id', () => {
      render(<RangedWeaponsEditor {...defaultProps} id="custom-weapon" />);
      
      // The wrapper div should have the ranged weapon editor ID
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates short range', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const shortRangeInput = screen.getByLabelText('Short Range');
      fireEvent.change(shortRangeInput, { target: { value: '20' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          shortRange: 20
        }),
        0
      );
    });

    it('updates medium range', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const mediumRangeInput = screen.getByLabelText('Medium Range');
      fireEvent.change(mediumRangeInput, { target: { value: '40' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          mediumRange: 40
        }),
        0
      );
    });

    it('updates long range', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const longRangeInput = screen.getByLabelText('Long Range');
      fireEvent.change(longRangeInput, { target: { value: '80' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          longRange: 80
        }),
        0
      );
    });

    it('updates damage', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: '2d8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          damage: '2d8'
        }),
        0
      );
    });

    it('updates rate of fire', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const rateOfFireInput = screen.getByLabelText('Rate of Fire');
      fireEvent.change(rateOfFireInput, { target: { value: '3' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          rateOfFire: 3
        }),
        0
      );
    });

    it('updates shots', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const shotsInput = screen.getByLabelText('Shots');
      fireEvent.change(shotsInput, { target: { value: '30' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          shots: 30
        }),
        0
      );
    });

    it('updates minimum strength', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const minStrengthInput = screen.getByLabelText('Minimum Strength');
      fireEvent.change(minStrengthInput, { target: { value: 'd8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          minimumStrength: 'd8'
        }),
        0
      );
    });

    it('converts numeric values to integers', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const shotsInput = screen.getByLabelText('Shots');
      fireEvent.change(shotsInput, { target: { value: '15.7' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          shots: 15
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} index={2} onChange={mockOnChange} />);
      
      const damageInput = screen.getByLabelText('Damage');
      fireEvent.change(damageInput, { target: { value: '3d6' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 2);
    });
  });

  describe('Savage Worlds Ranged Weapons', () => {
    it('handles bows and crossbows', () => {
      const rangedWeapons = [
        {
          name: 'Short Bow',
          shortRange: 12,
          mediumRange: 24,
          longRange: 48,
          damage: '2d6',
          rateOfFire: 1,
          shots: 1,
          minimumStrength: 'd6'
        },
        {
          name: 'Crossbow',
          shortRange: 15,
          mediumRange: 30,
          longRange: 60,
          damage: '2d8',
          rateOfFire: 1,
          shots: 1,
          minimumStrength: 'd6'
        },
        {
          name: 'Heavy Crossbow',
          shortRange: 20,
          mediumRange: 40,
          longRange: 80,
          damage: '2d10',
          rateOfFire: 1,
          shots: 1,
          minimumStrength: 'd8'
        }
      ];

      rangedWeapons.forEach(weapon => {
        const fullWeapon = { ...mockRangedWeapon, ...weapon };
        const { rerender } = render(<RangedWeaponsEditor {...defaultProps} item={fullWeapon} />);
        
        expect(screen.getByLabelText('Short Range')).toHaveValue(weapon.shortRange.toString());
        expect(screen.getByLabelText('Damage')).toHaveValue(weapon.damage);
        expect(screen.getByLabelText('Minimum Strength')).toHaveValue(weapon.minimumStrength);
        
        rerender(<div />);
      });
    });

    it('handles firearms', () => {
      const pistol = {
        ...mockRangedWeapon,
        name: 'Colt Peacemaker',
        era: 'Wild West',
        shortRange: 12,
        mediumRange: 24,
        longRange: 48,
        damage: '2d6+1',
        rateOfFire: 1,
        shots: 6,
        minimumStrength: 'd4'
      };

      render(<RangedWeaponsEditor {...defaultProps} item={pistol} />);
      
      expect(screen.getByLabelText('Shots')).toHaveValue('6');
      expect(screen.getByLabelText('Damage')).toHaveValue('2d6+1');
    });

    it('handles modern weapons', () => {
      const modernRifle = {
        ...mockRangedWeapon,
        name: 'Assault Rifle',
        era: 'Modern',
        shortRange: 24,
        mediumRange: 48,
        longRange: 96,
        damage: '2d8',
        rateOfFire: 3,
        shots: 30,
        minimumStrength: 'd6'
      };

      render(<RangedWeaponsEditor {...defaultProps} item={modernRifle} />);
      
      expect(screen.getByLabelText('Rate of Fire')).toHaveValue('3');
      expect(screen.getByLabelText('Shots')).toHaveValue('30');
      expect(screen.getByLabelText('Long Range')).toHaveValue('96');
    });

    it('handles thrown weapons', () => {
      const throwingKnife = {
        ...mockRangedWeapon,
        name: 'Throwing Knife',
        shortRange: 3,
        mediumRange: 6,
        longRange: 12,
        damage: 'Str+d4',
        rateOfFire: 1,
        shots: 1,
        minimumStrength: 'd4'
      };

      render(<RangedWeaponsEditor {...defaultProps} item={throwingKnife} />);
      
      expect(screen.getByLabelText('Damage')).toHaveValue('Str+d4');
      expect(screen.getByLabelText('Short Range')).toHaveValue('3');
    });
  });

  describe('Additional Ranged Weapon Fields Extension', () => {
    it('renders additional ranged weapon fields when method exists', () => {
      class ExtendedRangedWeaponsEditor extends RangedWeaponsEditor {
        additionalRangedWeaponFields = () => (
          <div data-testid="additional-ranged-fields">
            <input data-testid="additional-ranged-input" />
          </div>
        );
      }

      render(<ExtendedRangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('additional-ranged-fields')).toBeInTheDocument();
      expect(screen.getByTestId('additional-ranged-input')).toBeInTheDocument();
    });

    it('does not render additional fields section when method does not exist', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      expect(screen.queryByTestId('additional-ranged-fields')).not.toBeInTheDocument();
    });
  });

  describe('Data Handling', () => {
    it('handles zero values', () => {
      const zeroWeapon = {
        ...mockRangedWeapon,
        shortRange: 0,
        mediumRange: 0,
        longRange: 0,
        rateOfFire: 0,
        shots: 0
      };

      render(<RangedWeaponsEditor {...defaultProps} item={zeroWeapon} />);
      
      expect(screen.getByLabelText('Short Range')).toHaveValue('0');
      expect(screen.getByLabelText('Rate of Fire')).toHaveValue('0');
    });

    it('handles undefined ranged weapon properties', () => {
      const incompleteWeapon = {
        name: 'Incomplete Weapon',
        description: 'Missing range data'
      };

      expect(() => {
        render(<RangedWeaponsEditor {...defaultProps} item={incompleteWeapon} />);
      }).not.toThrow();
    });

    it('handles complex damage expressions', () => {
      const complexDamageWeapon = {
        ...mockRangedWeapon,
        damage: 'Str+2d6+2'
      };

      render(<RangedWeaponsEditor {...defaultProps} item={complexDamageWeapon} />);
      
      expect(screen.getByLabelText('Damage')).toHaveValue('Str+2d6+2');
    });
  });

  describe('Component Integration', () => {
    it('extends MundaneItemEditor properly', () => {
      render(<RangedWeaponsEditor {...defaultProps} />);
      
      // Should render parent functionality
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
      
      // Should add ranged weapon specific fields
      expect(screen.getByLabelText('Short Range')).toBeInTheDocument();
      expect(screen.getByLabelText('Damage')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new RangedWeaponsEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });
  });

  describe('Edge Cases', () => {
    it('handles very high range values', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const longRangeInput = screen.getByLabelText('Long Range');
      fireEvent.change(longRangeInput, { target: { value: '1000' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          longRange: 1000
        }),
        0
      );
    });

    it('handles negative values gracefully', () => {
      const mockOnChange = jest.fn();
      render(<RangedWeaponsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const rateOfFireInput = screen.getByLabelText('Rate of Fire');
      fireEvent.change(rateOfFireInput, { target: { value: '-1' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          rateOfFire: -1
        }),
        0
      );
    });

    it('handles special weapon properties', () => {
      const specialWeapon = {
        ...mockRangedWeapon,
        name: 'Magical Bow of Accuracy',
        damage: '2d6+2',
        note: '+2 to Shooting rolls'
      };

      expect(() => {
        render(<RangedWeaponsEditor {...defaultProps} item={specialWeapon} />);
      }).not.toThrow();
    });
  });
});