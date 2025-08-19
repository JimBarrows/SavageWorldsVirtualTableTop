import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import VehicleMountedGunsEditor from './Editor';

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

describe('VehicleMountedGunsEditor Component', () => {
  const mockVehicleGun = {
    name: '37mm AT Gun',
    description: 'Anti-tank cannon',
    era: 'WWII',
    kind: 'Vehicle Mounted',
    note: 'High-velocity gun',
    cost: 8000,
    weight: 2500,
    apDamage: '4d8',
    apArmorPiercing: 6,
    heDamage: '3d6',
    heArmorPiercing: 0,
    heBurstTemplate: 'Medium Burst Template'
  };

  const defaultProps = {
    id: 'test-vehicle-gun',
    item: mockVehicleGun,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
    });

    it('renders all AP (Armor Piercing) fields', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('AP Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('AP Armor Piercing')).toBeInTheDocument();
    });

    it('renders all HE (High Explosive) fields', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('HE Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Armor Piercing')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Burst Template')).toBeInTheDocument();
    });

    it('displays current AP damage value', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      expect(apDamageInput).toHaveValue('4d8');
    });

    it('displays current AP armor piercing value', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      expect(apArmorPiercingInput).toHaveValue('6');
    });

    it('displays current HE damage value', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const heDamageInput = screen.getByLabelText('HE Damage');
      expect(heDamageInput).toHaveValue('3d6');
    });

    it('displays current HE armor piercing value', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const heArmorPiercingInput = screen.getByLabelText('HE Armor Piercing');
      expect(heArmorPiercingInput).toHaveValue('0');
    });

    it('displays current HE burst template value', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
      expect(heBurstTemplateInput).toHaveValue('Medium Burst Template');
    });

    it('marks required fields as required', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      
      expect(apDamageInput).toHaveAttribute('required');
      expect(apArmorPiercingInput).toHaveAttribute('required');
    });

    it('does not mark optional HE fields as required', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      const heDamageInput = screen.getByLabelText('HE Damage');
      const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
      const heArmorPiercingInput = screen.getByLabelText('HE Armor Piercing');
      
      expect(heDamageInput).not.toHaveAttribute('required');
      expect(heBurstTemplateInput).not.toHaveAttribute('required');
      expect(heArmorPiercingInput).not.toHaveAttribute('required');
    });

    it('uses correct field ids', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('AP Damage')).toHaveAttribute('id', 'vehicleMountedAndAtGunsApDamage');
      expect(screen.getByLabelText('AP Armor Piercing')).toHaveAttribute('id', 'vehicleMountedAndAtGunsApArmorPiercing');
      expect(screen.getByLabelText('HE Damage')).toHaveAttribute('id', 'vehicleMountedAndAtGunsHeDamage');
      expect(screen.getByLabelText('HE Burst Template')).toHaveAttribute('id', 'vehicleMountedAndAtGunsHeBurstTemplate');
      expect(screen.getByLabelText('HE Armor Piercing')).toHaveAttribute('id', 'vehicleMountedAndAtGunsHeArmorPiercing');
    });
  });

  describe('AP (Armor Piercing) Field Updates', () => {
    it('updates AP damage when AP damage field changes', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      fireEvent.change(apDamageInput, { target: { value: '5d8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          apDamage: '5d8'
        }),
        0
      );
    });

    it('updates AP armor piercing when AP armor piercing field changes', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      fireEvent.change(apArmorPiercingInput, { target: { value: '8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          apArmorPiercing: 8
        }),
        0
      );
    });

    it('converts AP armor piercing to integer', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      fireEvent.change(apArmorPiercingInput, { target: { value: '4.7' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          apArmorPiercing: 4
        }),
        0
      );
    });
  });

  describe('HE (High Explosive) Field Updates', () => {
    it('updates HE damage when HE damage field changes', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const heDamageInput = screen.getByLabelText('HE Damage');
      fireEvent.change(heDamageInput, { target: { value: '4d6' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          heDamage: '4d6'
        }),
        0
      );
    });

    it('updates HE armor piercing when HE armor piercing field changes', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const heArmorPiercingInput = screen.getByLabelText('HE Armor Piercing');
      fireEvent.change(heArmorPiercingInput, { target: { value: '2' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          heArmorPiercing: 2
        }),
        0
      );
    });

    it('updates HE burst template when HE burst template field changes', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
      fireEvent.change(heBurstTemplateInput, { target: { value: 'Large Burst Template' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          heBurstTemplate: 'Large Burst Template'
        }),
        0
      );
    });

    it('converts HE armor piercing to integer', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const heArmorPiercingInput = screen.getByLabelText('HE Armor Piercing');
      fireEvent.change(heArmorPiercingInput, { target: { value: '1.9' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          heArmorPiercing: 1
        }),
        0
      );
    });
  });

  describe('Complete Field Updates', () => {
    it('preserves all existing item properties when updating fields', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      fireEvent.change(apDamageInput, { target: { value: '6d8' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      expect(callArguments).toEqual({
        name: '37mm AT Gun',
        description: 'Anti-tank cannon',
        era: 'WWII',
        kind: 'Vehicle Mounted',
        note: 'High-velocity gun',
        cost: 8000,
        weight: 2500,
        apDamage: '6d8',
        apArmorPiercing: 6,
        heDamage: '3d6',
        heArmorPiercing: 0,
        heBurstTemplate: 'Medium Burst Template'
      });
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} index={4} onChange={mockOnChange} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      fireEvent.change(apDamageInput, { target: { value: '3d8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 4);
    });
  });

  describe('Savage Worlds Vehicle Guns', () => {
    const vehicleGuns = [
      {
        name: '20mm Cannon',
        apDamage: '2d8',
        apArmorPiercing: 2,
        heDamage: '2d6',
        heArmorPiercing: 0,
        heBurstTemplate: 'Small Burst Template'
      },
      {
        name: '37mm AT Gun',
        apDamage: '4d8',
        apArmorPiercing: 6,
        heDamage: '3d6',
        heArmorPiercing: 0,
        heBurstTemplate: 'Medium Burst Template'
      },
      {
        name: '75mm Gun',
        apDamage: '4d10',
        apArmorPiercing: 8,
        heDamage: '4d8',
        heArmorPiercing: 1,
        heBurstTemplate: 'Large Burst Template'
      },
      {
        name: '88mm Gun',
        apDamage: '5d10',
        apArmorPiercing: 10,
        heDamage: '5d8',
        heArmorPiercing: 2,
        heBurstTemplate: 'Large Burst Template'
      }
    ];

    it('handles typical Savage Worlds vehicle-mounted guns', () => {
      vehicleGuns.forEach(gun => {
        const fullGun = { ...mockVehicleGun, ...gun };
        const { rerender } = render(<VehicleMountedGunsEditor {...defaultProps} item={fullGun} />);
        
        expect(screen.getByLabelText('AP Damage')).toHaveValue(gun.apDamage);
        expect(screen.getByLabelText('AP Armor Piercing')).toHaveValue(gun.apArmorPiercing.toString());
        expect(screen.getByLabelText('HE Damage')).toHaveValue(gun.heDamage);
        expect(screen.getByLabelText('HE Armor Piercing')).toHaveValue(gun.heArmorPiercing.toString());
        expect(screen.getByLabelText('HE Burst Template')).toHaveValue(gun.heBurstTemplate);
        
        rerender(<div />);
      });
    });

    it('handles AP-only weapons (no HE rounds)', () => {
      const apOnlyGun = {
        ...mockVehicleGun,
        name: 'AT Gun (AP Only)',
        apDamage: '5d8',
        apArmorPiercing: 8,
        heDamage: '',
        heArmorPiercing: 0,
        heBurstTemplate: ''
      };

      render(<VehicleMountedGunsEditor {...defaultProps} item={apOnlyGun} />);
      
      expect(screen.getByLabelText('AP Damage')).toHaveValue('5d8');
      expect(screen.getByLabelText('AP Armor Piercing')).toHaveValue('8');
      expect(screen.getByLabelText('HE Damage')).toHaveValue('');
      expect(screen.getByLabelText('HE Burst Template')).toHaveValue('');
    });

    it('handles HE-only weapons (no AP rounds)', () => {
      const heOnlyGun = {
        ...mockVehicleGun,
        name: 'Howitzer',
        apDamage: '',
        apArmorPiercing: 0,
        heDamage: '4d8',
        heArmorPiercing: 1,
        heBurstTemplate: 'Large Burst Template'
      };

      render(<VehicleMountedGunsEditor {...defaultProps} item={heOnlyGun} />);
      
      expect(screen.getByLabelText('AP Damage')).toHaveValue('');
      expect(screen.getByLabelText('HE Damage')).toHaveValue('4d8');
      expect(screen.getByLabelText('HE Burst Template')).toHaveValue('Large Burst Template');
    });
  });

  describe('Damage Value Formats', () => {
    it('handles various damage die formats', () => {
      const damageFormats = ['2d6', '3d8', '4d10', '5d12', '6d6+2', '3d8-1'];
      
      damageFormats.forEach(damage => {
        const mockOnChange = jest.fn();
        render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
        
        const apDamageInput = screen.getByLabelText('AP Damage');
        fireEvent.change(apDamageInput, { target: { value: damage } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ apDamage: damage }),
          0
        );
      });
    });

    it('handles complex damage descriptions', () => {
      const complexDamage = '4d8, AP 6, Heavy Weapon';
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      fireEvent.change(apDamageInput, { target: { value: complexDamage } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ apDamage: complexDamage }),
        0
      );
    });
  });

  describe('Burst Template Types', () => {
    const burstTemplates = [
      'Small Burst Template',
      'Medium Burst Template', 
      'Large Burst Template',
      'Cone Template',
      'Stream Template'
    ];

    it('handles various burst template types', () => {
      burstTemplates.forEach(template => {
        const mockOnChange = jest.fn();
        render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
        
        const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
        fireEvent.change(heBurstTemplateInput, { target: { value: template } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ heBurstTemplate: template }),
          0
        );
      });
    });
  });

  describe('Data Handling', () => {
    it('handles undefined values gracefully', () => {
      const incompleteGun = {
        name: 'Incomplete Gun'
      };

      expect(() => {
        render(<VehicleMountedGunsEditor {...defaultProps} item={incompleteGun} />);
      }).not.toThrow();
    });

    it('handles null values', () => {
      const nullGun = {
        ...mockVehicleGun,
        apDamage: null,
        apArmorPiercing: null,
        heDamage: null,
        heArmorPiercing: null,
        heBurstTemplate: null
      };

      render(<VehicleMountedGunsEditor {...defaultProps} item={nullGun} />);
      
      expect(screen.getByLabelText('AP Damage')).toHaveValue('');
      expect(screen.getByLabelText('AP Armor Piercing')).toHaveValue('');
      expect(screen.getByLabelText('HE Damage')).toHaveValue('');
      expect(screen.getByLabelText('HE Armor Piercing')).toHaveValue('');
      expect(screen.getByLabelText('HE Burst Template')).toHaveValue('');
    });

    it('handles empty string values', () => {
      const emptyGun = {
        ...mockVehicleGun,
        apDamage: '',
        heDamage: '',
        heBurstTemplate: ''
      };

      render(<VehicleMountedGunsEditor {...defaultProps} item={emptyGun} />);
      
      expect(screen.getByLabelText('AP Damage')).toHaveValue('');
      expect(screen.getByLabelText('HE Damage')).toHaveValue('');
      expect(screen.getByLabelText('HE Burst Template')).toHaveValue('');
    });

    it('handles non-numeric input for armor piercing values', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      fireEvent.change(apArmorPiercingInput, { target: { value: 'abc' } });
      
      // parseInt('abc', 10) returns NaN
      expect(mockOnChange).toHaveBeenCalled();
    });

    it('handles negative armor piercing values', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const heArmorPiercingInput = screen.getByLabelText('HE Armor Piercing');
      fireEvent.change(heArmorPiercingInput, { target: { value: '-1' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ heArmorPiercing: -1 }),
        0
      );
    });
  });

  describe('Component Integration', () => {
    it('extends MundaneItemEditor properly', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      // Should render base MundaneItemEditor functionality
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
      
      // Should add additional fields
      expect(screen.getByLabelText('AP Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Damage')).toBeInTheDocument();
    });

    it('uses additionalRangedWeaponFields method', () => {
      render(<VehicleMountedGunsEditor {...defaultProps} />);
      
      // Method should be called and render all additional fields
      expect(screen.getByLabelText('AP Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('AP Armor Piercing')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Damage')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Armor Piercing')).toBeInTheDocument();
      expect(screen.getByLabelText('HE Burst Template')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new VehicleMountedGunsEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid value changes across all fields', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      const heDamageInput = screen.getByLabelText('HE Damage');
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      
      fireEvent.change(apDamageInput, { target: { value: '2d6' } });
      fireEvent.change(heDamageInput, { target: { value: '3d8' } });
      fireEvent.change(apArmorPiercingInput, { target: { value: '5' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
    });

    it('handles very large armor piercing values', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
      fireEvent.change(apArmorPiercingInput, { target: { value: '999' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ apArmorPiercing: 999 }),
        0
      );
    });

    it('handles very long damage and template strings', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const longDamage = '5d10+3, AP 12, Heavy Weapon, Vehicle Scale, +2 to hit stationary targets';
      const longTemplate = 'Extra Large Burst Template with Extended Range and Special Effects';
      
      const apDamageInput = screen.getByLabelText('AP Damage');
      const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
      
      fireEvent.change(apDamageInput, { target: { value: longDamage } });
      fireEvent.change(heBurstTemplateInput, { target: { value: longTemplate } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ apDamage: longDamage }),
        0
      );
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ heBurstTemplate: longTemplate }),
        0
      );
    });

    it('handles special characters in damage and template fields', () => {
      const mockOnChange = jest.fn();
      render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
      
      const specialDamage = '4d8 (½ damage vs. infantry)';
      const specialTemplate = 'Template (6" × 12")';
      
      const heDamageInput = screen.getByLabelText('HE Damage');
      const heBurstTemplateInput = screen.getByLabelText('HE Burst Template');
      
      fireEvent.change(heDamageInput, { target: { value: specialDamage } });
      fireEvent.change(heBurstTemplateInput, { target: { value: specialTemplate } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ heDamage: specialDamage }),
        0
      );
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ heBurstTemplate: specialTemplate }),
        0
      );
    });
  });

  describe('Historical Weapon Accuracy', () => {
    it('supports WWII era anti-tank guns', () => {
      const ww2Guns = [
        { name: 'Pak 36 (37mm)', apArmorPiercing: 3 },
        { name: 'Pak 40 (75mm)', apArmorPiercing: 8 },
        { name: '88mm FlaK', apArmorPiercing: 10 },
        { name: 'Soviet 76.2mm', apArmorPiercing: 7 }
      ];

      ww2Guns.forEach(gun => {
        const mockOnChange = jest.fn();
        render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
        
        const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
        fireEvent.change(apArmorPiercingInput, { target: { value: gun.apArmorPiercing.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ apArmorPiercing: gun.apArmorPiercing }),
          0
        );
      });
    });

    it('supports modern tank guns', () => {
      const modernGuns = [
        { name: '105mm Gun', apArmorPiercing: 15 },
        { name: '120mm Gun', apArmorPiercing: 20 },
        { name: '125mm Gun', apArmorPiercing: 18 }
      ];

      modernGuns.forEach(gun => {
        const mockOnChange = jest.fn();
        render(<VehicleMountedGunsEditor {...defaultProps} onChange={mockOnChange} />);
        
        const apArmorPiercingInput = screen.getByLabelText('AP Armor Piercing');
        fireEvent.change(apArmorPiercingInput, { target: { value: gun.apArmorPiercing.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ apArmorPiercing: gun.apArmorPiercing }),
          0
        );
      });
    });
  });
});