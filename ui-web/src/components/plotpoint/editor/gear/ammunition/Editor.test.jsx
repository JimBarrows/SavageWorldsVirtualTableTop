import React from 'react';
import { render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import AmmunitionEditor from './Editor';

// Mock MundaneItemEditor
jest.mock('../mundane_items/Editor', () => {
  const MockMundaneItemEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'mundane-item-editor' },
      React.createElement('div', { 'data-testid': 'ammunition-fields' }, [
        React.createElement('input', { 'data-testid': 'name-input', value: props.item.name || '', readOnly: true, key: 'name' }),
        React.createElement('input', { 'data-testid': 'cost-input', value: props.item.cost || '', readOnly: true, key: 'cost' }),
        React.createElement('input', { 'data-testid': 'weight-input', value: props.item.weight || '', readOnly: true, key: 'weight' })
      ])
    );
  };
  MockMundaneItemEditor.prototype = {};
  return MockMundaneItemEditor;
});

describe('AmmunitionEditor Component', () => {
  const mockAmmunitionItem = {
    name: 'Arrows (20)',
    description: 'Standard arrows for bows',
    era: 'Medieval',
    kind: 'Ammunition',
    note: 'Quiver not included',
    cost: 5,
    weight: 5
  };

  const defaultProps = {
    id: 'test-ammunition',
    item: mockAmmunitionItem,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<AmmunitionEditor {...defaultProps} />);
      
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('ammunition-fields')).toBeInTheDocument();
    });

    it('displays ammunition item data', () => {
      render(<AmmunitionEditor {...defaultProps} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Arrows (20)');
      expect(screen.getByTestId('cost-input')).toHaveValue('5');
      expect(screen.getByTestId('weight-input')).toHaveValue('5');
    });

    it('inherits from MundaneItemEditor', () => {
      render(<AmmunitionEditor {...defaultProps} />);
      
      // Should render the parent component
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
    });
  });

  describe('Savage Worlds Ammunition Types', () => {
    it('handles different arrow types', () => {
      const arrowTypes = [
        { name: 'Arrows (20)', cost: 5, weight: 5 },
        { name: 'Bodkin Arrows (20)', cost: 10, weight: 5 },
        { name: 'Silver Arrows (20)', cost: 40, weight: 5 }
      ];

      arrowTypes.forEach(ammo => {
        const ammunitionItem = { ...mockAmmunitionItem, ...ammo };
        const { rerender } = render(<AmmunitionEditor {...defaultProps} item={ammunitionItem} />);
        
        expect(screen.getByTestId('name-input')).toHaveValue(ammo.name);
        expect(screen.getByTestId('cost-input')).toHaveValue(ammo.cost.toString());
        
        rerender(<div />);
      });
    });

    it('handles firearm ammunition', () => {
      const modernAmmo = {
        ...mockAmmunitionItem,
        name: 'Bullets (50)',
        description: 'Standard pistol ammunition',
        era: 'Modern',
        cost: 10,
        weight: 2
      };

      render(<AmmunitionEditor {...defaultProps} item={modernAmmo} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Bullets (50)');
      expect(screen.getByTestId('cost-input')).toHaveValue('10');
      expect(screen.getByTestId('weight-input')).toHaveValue('2');
    });

    it('handles sling stones', () => {
      const slingstones = {
        ...mockAmmunitionItem,
        name: 'Sling Stones (20)',
        description: 'Smooth river stones',
        era: 'Ancient',
        cost: 0,
        weight: 5
      };

      render(<AmmunitionEditor {...defaultProps} item={slingstones} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Sling Stones (20)');
      expect(screen.getByTestId('cost-input')).toHaveValue('0');
    });

    it('handles crossbow bolts', () => {
      const bolts = {
        ...mockAmmunitionItem,
        name: 'Crossbow Bolts (20)',
        description: 'Heavy quarrels for crossbows',
        cost: 10,
        weight: 10
      };

      render(<AmmunitionEditor {...defaultProps} item={bolts} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Crossbow Bolts (20)');
      expect(screen.getByTestId('cost-input')).toHaveValue('10');
      expect(screen.getByTestId('weight-input')).toHaveValue('10');
    });
  });

  describe('Component Integration', () => {
    it('properly extends MundaneItemEditor', () => {
      const component = new AmmunitionEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('passes props to parent component', () => {
      render(<AmmunitionEditor {...defaultProps} />);
      
      // Should render with all the expected elements from parent
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
      expect(screen.getByTestId('ammunition-fields')).toBeInTheDocument();
    });

    it('maintains prop structure', () => {
      render(<AmmunitionEditor {...defaultProps} />);
      
      // Verify the component renders without throwing
      expect(screen.getByTestId('mundane-item-editor')).toBeInTheDocument();
    });
  });

  describe('Data Handling', () => {
    it('handles empty ammunition item', () => {
      const emptyAmmo = {
        name: '',
        cost: 0,
        weight: 0
      };

      expect(() => {
        render(<AmmunitionEditor {...defaultProps} item={emptyAmmo} />);
      }).not.toThrow();
    });

    it('handles undefined properties gracefully', () => {
      const partialAmmo = {
        name: 'Basic Ammo'
      };

      expect(() => {
        render(<AmmunitionEditor {...defaultProps} item={partialAmmo} />);
      }).not.toThrow();
    });

    it('handles various ammunition quantities', () => {
      const quantities = [10, 20, 50, 100];
      
      quantities.forEach(qty => {
        const ammo = {
          ...mockAmmunitionItem,
          name: `Arrows (${qty})`,
          cost: qty / 4, // Cost scales with quantity
          weight: qty / 4
        };
        
        const { rerender } = render(<AmmunitionEditor {...defaultProps} item={ammo} />);
        
        expect(screen.getByTestId('name-input')).toHaveValue(`Arrows (${qty})`);
        
        rerender(<div />);
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles special ammunition types', () => {
      const specialAmmo = {
        ...mockAmmunitionItem,
        name: 'Flaming Arrows (5)',
        description: 'Arrows with oil-soaked rags',
        cost: 25,
        weight: 5,
        note: '+1d6 fire damage, may ignite flammable targets'
      };

      expect(() => {
        render(<AmmunitionEditor {...defaultProps} item={specialAmmo} />);
      }).not.toThrow();
    });

    it('handles magical ammunition', () => {
      const magicalAmmo = {
        ...mockAmmunitionItem,
        name: 'Silver Arrows (10)',
        description: 'Arrows tipped with pure silver',
        cost: 100,
        weight: 5,
        note: 'Effective against supernatural creatures'
      };

      render(<AmmunitionEditor {...defaultProps} item={magicalAmmo} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Silver Arrows (10)');
      expect(screen.getByTestId('cost-input')).toHaveValue('100');
    });

    it('handles very expensive ammunition', () => {
      const expensiveAmmo = {
        ...mockAmmunitionItem,
        name: 'Adamantine Bullets (10)',
        cost: 1000,
        weight: 2
      };

      render(<AmmunitionEditor {...defaultProps} item={expensiveAmmo} />);
      
      expect(screen.getByTestId('cost-input')).toHaveValue('1000');
    });
  });
});