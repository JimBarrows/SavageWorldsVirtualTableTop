import React from 'react';
import { render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import DramaticPersonae from './DramaticPersonae';

describe('DramaticPersonae Appendix Component', () => {
  const mockCharacters = [
    {
      name: 'Lord Ashworth',
      description: 'The mansion\'s former owner',
      background: 'Antagonist'
    },
    {
      name: 'Sarah Mills', 
      description: 'A local historian',
      background: 'Ally'
    },
    {
      name: 'Ghost Butler',
      description: 'Undead servant',
      background: 'Minor NPC'
    }
  ];

  const defaultProps = {
    id: 'test-dramatic-personae',
    characters: mockCharacters
  };

  describe('when characters are present', () => {
    test('displays appendix title', () => {
      render(<DramaticPersonae {...defaultProps} />);
      expect(screen.getByText('Dramatic Personae')).toBeInTheDocument();
    });

    test('displays all characters with their details', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      // Verify all character names are displayed
      expect(screen.getByText('Lord Ashworth')).toBeInTheDocument();
      expect(screen.getByText('Sarah Mills')).toBeInTheDocument();
      expect(screen.getByText('Ghost Butler')).toBeInTheDocument();
      
      // Verify character descriptions are displayed
      expect(screen.getByText('The mansion\'s former owner')).toBeInTheDocument();
      expect(screen.getByText('A local historian')).toBeInTheDocument();
      expect(screen.getByText('Undead servant')).toBeInTheDocument();
    });

    test('displays character backgrounds/roles', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      expect(screen.getByText('Antagonist')).toBeInTheDocument();
      expect(screen.getByText('Ally')).toBeInTheDocument();
      expect(screen.getByText('Minor NPC')).toBeInTheDocument();
    });

    test('provides link to edit characters section', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      const editLink = screen.getByText(/Edit Characters/i);
      expect(editLink).toBeInTheDocument();
      expect(editLink.closest('a, button')).toBeInTheDocument();
    });

    test('displays read-only character information', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      // Verify no editable form fields are present
      expect(screen.queryByRole('textbox')).not.toBeInTheDocument();
      expect(screen.queryByRole('combobox')).not.toBeInTheDocument();
      
      // Verify no edit/save/delete buttons for individual characters
      expect(screen.queryByText(/Save/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/Delete/i)).not.toBeInTheDocument();
    });
  });

  describe('when no characters are present', () => {
    const emptyProps = {
      id: 'test-dramatic-personae',
      characters: []
    };

    test('displays appendix title', () => {
      render(<DramaticPersonae {...emptyProps} />);
      expect(screen.getByText('Dramatic Personae')).toBeInTheDocument();
    });

    test('displays empty state message', () => {
      render(<DramaticPersonae {...emptyProps} />);
      expect(screen.getByText('No characters added to this plot point')).toBeInTheDocument();
    });

    test('still provides link to add characters', () => {
      render(<DramaticPersonae {...emptyProps} />);
      
      const editLink = screen.getByText(/Edit Characters/i);
      expect(editLink).toBeInTheDocument();
    });
  });

  describe('component structure and accessibility', () => {
    test('renders with correct test id', () => {
      render(<DramaticPersonae {...defaultProps} />);
      expect(screen.getByTestId('dramatic-personae-appendix')).toBeInTheDocument();
    });

    test('has proper semantic structure', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      // Should have proper heading hierarchy
      const heading = screen.getByRole('heading', { name: /Dramatic Personae/i });
      expect(heading).toBeInTheDocument();
      
      // Should have a list structure for characters
      const list = screen.getByRole('list');
      expect(list).toBeInTheDocument();
    });

    test('handles undefined characters gracefully', () => {
      const propsWithUndefined = {
        id: 'test-dramatic-personae',
        characters: undefined
      };
      
      expect(() => render(<DramaticPersonae {...propsWithUndefined} />)).not.toThrow();
      expect(screen.getByText('No characters added to this plot point')).toBeInTheDocument();
    });
  });

  describe('character display format', () => {
    test('displays character information in consistent format', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      // Each character should be in its own container
      const characterItems = screen.getAllByTestId(/character-summary/);
      expect(characterItems).toHaveLength(3);
    });

    test('displays character name prominently', () => {
      render(<DramaticPersonae {...defaultProps} />);
      
      // Character names should be displayed as headings or prominent text
      const lordAshworth = screen.getByText('Lord Ashworth');
      expect(lordAshworth.tagName).toMatch(/^(H[1-6]|STRONG|B)$/);
    });
  });
});