import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Navigation from './Navigation';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components/distribution/bootstrap/components/Nav', () => {
  return function MockNav({ children, id, position, look }) {
    return (
      <nav data-testid="nav" id={id} data-position={position} data-look={look}>
        {children}
      </nav>
    );
  };
});

jest.mock('bootstrap-react-components/distribution/bootstrap/components/Nav/NavItem', () => {
  return function MockNavItem({ id, onClick, label, state }) {
    return (
      <button
        data-testid="nav-item"
        id={id}
        onClick={onClick}
        data-state={state}
        data-label={label}
      >
        {label}
      </button>
    );
  };
});

describe('Navigation Component', () => {
  const defaultProps = {
    id: 'test-nav',
    navigateTo: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<Navigation {...defaultProps} />);
      
      expect(screen.getByTestId('nav')).toBeInTheDocument();
      expect(screen.getByTestId('nav')).toHaveAttribute('id', 'Navigation-test-nav');
    });

    it('renders with correct nav configuration', () => {
      render(<Navigation {...defaultProps} />);
      
      const nav = screen.getByTestId('nav');
      expect(nav).toHaveAttribute('data-position', 'verticle');
      expect(nav).toHaveAttribute('data-look', 'tabs');
    });

    it('renders all Savage Worlds navigation items', () => {
      render(<Navigation {...defaultProps} />);
      
      const expectedSections = [
        'Basic Rules',
        'Setting Rules', 
        'Skills',
        'Edges',
        'Hindrances',
        'Gear',
        'Powers',
        'Races',
        'Beasts',
        'Characters'
      ];

      expectedSections.forEach(section => {
        expect(screen.getByRole('button', { name: section })).toBeInTheDocument();
      });
    });

    it('generates correct IDs for navigation items', () => {
      render(<Navigation {...defaultProps} id="main" />);
      
      const expectedIds = [
        'Navigation-main-basic-rules',
        'Navigation-main-setting-rules',
        'Navigation-main-skills',
        'Navigation-main-edges',
        'Navigation-main-hindrances',
        'Navigation-main-gear',
        'Navigation-main-powers',
        'Navigation-main-races',
        'Navigation-main-beasts',
        'Navigation-main-characters'
      ];

      expectedIds.forEach(id => {
        expect(document.getElementById(id)).toBeInTheDocument();
      });
    });

    it('sets Basic Rules as active by default', () => {
      render(<Navigation {...defaultProps} />);
      
      const basicRulesItem = screen.getByRole('button', { name: 'Basic Rules' });
      expect(basicRulesItem).toHaveAttribute('data-state', 'active');
      
      // All other items should be enabled (not active)
      const otherItems = screen.getAllByTestId('nav-item').filter(item => 
        item.getAttribute('data-label') !== 'Basic Rules'
      );
      
      otherItems.forEach(item => {
        expect(item).toHaveAttribute('data-state', 'enabled');
      });
    });
  });

  describe('Navigation State Management', () => {
    it('updates active state when navigation item is clicked', () => {
      render(<Navigation {...defaultProps} />);
      
      const skillsItem = screen.getByRole('button', { name: 'Skills' });
      fireEvent.click(skillsItem);
      
      expect(skillsItem).toHaveAttribute('data-state', 'active');
      
      // Basic Rules should no longer be active
      const basicRulesItem = screen.getByRole('button', { name: 'Basic Rules' });
      expect(basicRulesItem).toHaveAttribute('data-state', 'enabled');
    });

    it('maintains only one active navigation item at a time', () => {
      render(<Navigation {...defaultProps} />);
      
      // Click on Powers
      const powersItem = screen.getByRole('button', { name: 'Powers' });
      fireEvent.click(powersItem);
      
      // Only Powers should be active
      const allItems = screen.getAllByTestId('nav-item');
      const activeItems = allItems.filter(item => 
        item.getAttribute('data-state') === 'active'
      );
      
      expect(activeItems).toHaveLength(1);
      expect(activeItems[0]).toHaveAttribute('data-label', 'Powers');
    });

    it('handles navigation between all sections correctly', () => {
      render(<Navigation {...defaultProps} />);
      
      const sections = [
        'Setting Rules',
        'Edges', 
        'Hindrances',
        'Gear',
        'Races',
        'Beasts',
        'Characters'
      ];

      sections.forEach(section => {
        const item = screen.getByRole('button', { name: section });
        fireEvent.click(item);
        
        expect(item).toHaveAttribute('data-state', 'active');
        
        // Verify only this item is active
        const allItems = screen.getAllByTestId('nav-item');
        const activeItems = allItems.filter(item => 
          item.getAttribute('data-state') === 'active'
        );
        expect(activeItems).toHaveLength(1);
      });
    });

    it('can navigate back to previously visited sections', () => {
      render(<Navigation {...defaultProps} />);
      
      // Navigate to Skills
      const skillsItem = screen.getByRole('button', { name: 'Skills' });
      fireEvent.click(skillsItem);
      expect(skillsItem).toHaveAttribute('data-state', 'active');
      
      // Navigate to Gear  
      const gearItem = screen.getByRole('button', { name: 'Gear' });
      fireEvent.click(gearItem);
      expect(gearItem).toHaveAttribute('data-state', 'active');
      expect(skillsItem).toHaveAttribute('data-state', 'enabled');
      
      // Navigate back to Skills
      fireEvent.click(skillsItem);
      expect(skillsItem).toHaveAttribute('data-state', 'active');
      expect(gearItem).toHaveAttribute('data-state', 'enabled');
    });
  });

  describe('Integration with navigateTo prop', () => {
    it('calls navigateTo prop when navigation item is clicked', () => {
      const mockNavigateTo = jest.fn();
      render(<Navigation {...defaultProps} navigateTo={mockNavigateTo} />);
      
      const edgesItem = screen.getByRole('button', { name: 'Edges' });
      fireEvent.click(edgesItem);
      
      expect(mockNavigateTo).toHaveBeenCalledWith('Edges');
    });

    it('calls navigateTo with correct section names for all items', () => {
      const mockNavigateTo = jest.fn();
      render(<Navigation {...defaultProps} navigateTo={mockNavigateTo} />);
      
      const expectedCalls = [
        { section: 'Basic Rules', value: 'BasicRules' },
        { section: 'Setting Rules', value: 'SettingRules' },
        { section: 'Skills', value: 'Skills' },
        { section: 'Edges', value: 'Edges' },
        { section: 'Hindrances', value: 'Hindrances' },
        { section: 'Gear', value: 'Gear' },
        { section: 'Powers', value: 'Powers' },
        { section: 'Races', value: 'Races' },
        { section: 'Beasts', value: 'Beasts' },
        { section: 'Characters', value: 'Characters' }
      ];

      expectedCalls.forEach(({ section, value }, index) => {
        const item = screen.getByRole('button', { name: section });
        fireEvent.click(item);
        
        expect(mockNavigateTo).toHaveBeenNthCalledWith(index + 1, value);
      });
    });

    it('handles multiple rapid navigation clicks', () => {
      const mockNavigateTo = jest.fn();
      render(<Navigation {...defaultProps} navigateTo={mockNavigateTo} />);
      
      const skillsItem = screen.getByRole('button', { name: 'Skills' });
      const racesItem = screen.getByRole('button', { name: 'Races' });
      const gearItem = screen.getByRole('button', { name: 'Gear' });
      
      // Rapid clicking
      fireEvent.click(skillsItem);
      fireEvent.click(racesItem);
      fireEvent.click(gearItem);
      
      expect(mockNavigateTo).toHaveBeenCalledTimes(3);
      expect(mockNavigateTo).toHaveBeenNthCalledWith(1, 'Skills');
      expect(mockNavigateTo).toHaveBeenNthCalledWith(2, 'Races');
      expect(mockNavigateTo).toHaveBeenNthCalledWith(3, 'Gear');
      
      // Final state should be Gear
      expect(gearItem).toHaveAttribute('data-state', 'active');
    });

    it('maintains state consistency after navigateTo calls', () => {
      const mockNavigateTo = jest.fn();
      render(<Navigation {...defaultProps} navigateTo={mockNavigateTo} />);
      
      const hindrancesItem = screen.getByRole('button', { name: 'Hindrances' });
      fireEvent.click(hindrancesItem);
      
      expect(mockNavigateTo).toHaveBeenCalledWith('Hindrances');
      expect(hindrancesItem).toHaveAttribute('data-state', 'active');
      
      // Verify component state stayed consistent
      const allItems = screen.getAllByTestId('nav-item');
      const activeItems = allItems.filter(item => 
        item.getAttribute('data-state') === 'active'
      );
      expect(activeItems).toHaveLength(1);
    });
  });

  describe('Savage Worlds Domain Logic', () => {
    it('includes all core game sections', () => {
      render(<Navigation {...defaultProps} />);
      
      // Core rules sections
      expect(screen.getByRole('button', { name: 'Basic Rules' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Setting Rules' })).toBeInTheDocument();
      
      // Character creation
      expect(screen.getByRole('button', { name: 'Skills' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Edges' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Hindrances' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Races' })).toBeInTheDocument();
      
      // Equipment
      expect(screen.getByRole('button', { name: 'Gear' })).toBeInTheDocument();
      
      // Supernatural
      expect(screen.getByRole('button', { name: 'Powers' })).toBeInTheDocument();
      
      // NPCs and creatures
      expect(screen.getByRole('button', { name: 'Beasts' })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Characters' })).toBeInTheDocument();
    });

    it('organizes sections in logical game order', () => {
      render(<Navigation {...defaultProps} />);
      
      const navItems = screen.getAllByTestId('nav-item');
      const sectionOrder = navItems.map(item => item.getAttribute('data-label'));
      
      // Basic rules should come first
      expect(sectionOrder[0]).toBe('Basic Rules');
      expect(sectionOrder[1]).toBe('Setting Rules');
      
      // Character creation elements should be grouped
      const characterSections = ['Skills', 'Edges', 'Hindrances'];
      characterSections.forEach(section => {
        expect(sectionOrder).toContain(section);
      });
      
      // Powers should be available for arcane characters
      expect(sectionOrder).toContain('Powers');
      
      // Races for character creation
      expect(sectionOrder).toContain('Races');
      
      // Equipment for characters
      expect(sectionOrder).toContain('Gear');
      
      // NPCs and monsters for GM
      expect(sectionOrder).toContain('Beasts');
      expect(sectionOrder).toContain('Characters');
    });

    it('uses consistent naming convention with other game components', () => {
      const mockNavigateTo = jest.fn();
      render(<Navigation {...defaultProps} navigateTo={mockNavigateTo} />);
      
      // Test a few key sections to ensure naming consistency
      fireEvent.click(screen.getByRole('button', { name: 'Basic Rules' }));
      expect(mockNavigateTo).toHaveBeenCalledWith('BasicRules');
      
      fireEvent.click(screen.getByRole('button', { name: 'Setting Rules' }));
      expect(mockNavigateTo).toHaveBeenCalledWith('SettingRules');
    });
  });

  describe('Accessibility', () => {
    it('uses semantic navigation structure', () => {
      render(<Navigation {...defaultProps} />);
      
      const nav = screen.getByTestId('nav');
      expect(nav.tagName).toBe('NAV');
    });

    it('provides accessible button labels', () => {
      render(<Navigation {...defaultProps} />);
      
      const buttons = screen.getAllByRole('button');
      buttons.forEach(button => {
        expect(button).toHaveAccessibleName();
        expect(button.textContent.trim()).not.toBe('');
      });
    });

    it('indicates active state for screen readers', () => {
      render(<Navigation {...defaultProps} />);
      
      const activeItem = screen.getByRole('button', { name: 'Basic Rules' });
      expect(activeItem).toHaveAttribute('data-state', 'active');
      
      // Click another item and verify state change
      const skillsItem = screen.getByRole('button', { name: 'Skills' });
      fireEvent.click(skillsItem);
      
      expect(skillsItem).toHaveAttribute('data-state', 'active');
      expect(activeItem).toHaveAttribute('data-state', 'enabled');
    });
  });

  describe('Component Structure', () => {
    it('maintains consistent component ID pattern', () => {
      render(<Navigation {...defaultProps} id="editor" />);
      
      const nav = screen.getByTestId('nav');
      expect(nav).toHaveAttribute('id', 'Navigation-editor');
      
      // Check that nav items follow the pattern
      expect(document.getElementById('Navigation-editor-basic-rules')).toBeInTheDocument();
      expect(document.getElementById('Navigation-editor-skills')).toBeInTheDocument();
    });

    it('uses Bootstrap navigation components correctly', () => {
      render(<Navigation {...defaultProps} />);
      
      const nav = screen.getByTestId('nav');
      expect(nav).toHaveAttribute('data-position', 'verticle');
      expect(nav).toHaveAttribute('data-look', 'tabs');
    });

    it('handles empty or undefined props gracefully', () => {
      const mockNavigateTo = jest.fn();
      
      expect(() => {
        render(<Navigation id="test" navigateTo={mockNavigateTo} />);
      }).not.toThrow();
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid state changes correctly', () => {
      render(<Navigation {...defaultProps} />);
      
      const items = [
        'Skills',
        'Edges', 
        'Hindrances',
        'Gear',
        'Powers'
      ];
      
      // Rapidly click through items
      items.forEach(itemName => {
        const item = screen.getByRole('button', { name: itemName });
        fireEvent.click(item);
        expect(item).toHaveAttribute('data-state', 'active');
      });
      
      // Verify final state is correct
      const activeItems = screen.getAllByTestId('nav-item').filter(item => 
        item.getAttribute('data-state') === 'active'
      );
      expect(activeItems).toHaveLength(1);
      expect(activeItems[0]).toHaveAttribute('data-label', 'Powers');
    });

    it('maintains component stability during prop changes', () => {
      const { rerender } = render(<Navigation {...defaultProps} />);
      
      const originalNav = screen.getByTestId('nav');
      
      // Change navigateTo prop
      const newNavigateTo = jest.fn();
      rerender(<Navigation {...defaultProps} navigateTo={newNavigateTo} />);
      
      const updatedNav = screen.getByTestId('nav');
      expect(updatedNav).toBe(originalNav); // Same DOM element
      
      // Test that new prop function works
      fireEvent.click(screen.getByRole('button', { name: 'Races' }));
      expect(newNavigateTo).toHaveBeenCalledWith('Races');
    });

    it('preserves state across re-renders', () => {
      const { rerender } = render(<Navigation {...defaultProps} />);
      
      // Change active state
      const gearItem = screen.getByRole('button', { name: 'Gear' });
      fireEvent.click(gearItem);
      expect(gearItem).toHaveAttribute('data-state', 'active');
      
      // Re-render with same props
      rerender(<Navigation {...defaultProps} />);
      
      // State should be preserved
      const gearItemAfterRerender = screen.getByRole('button', { name: 'Gear' });
      expect(gearItemAfterRerender).toHaveAttribute('data-state', 'active');
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts valid prop types', () => {
      const validProps = {
        id: 'test-navigation',
        navigateTo: jest.fn()
      };
      
      expect(() => render(<Navigation {...validProps} />)).not.toThrow();
    });

    it('works with different ID formats', () => {
      const testIds = ['simple', 'with-dashes', 'with_underscores', 'CamelCase', '123numeric'];
      
      testIds.forEach(id => {
        expect(() => {
          const { unmount } = render(<Navigation id={id} navigateTo={jest.fn()} />);
          unmount();
        }).not.toThrow();
      });
    });
  });
});