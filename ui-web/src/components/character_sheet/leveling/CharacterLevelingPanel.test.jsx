import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import CharacterLevelingPanel from './CharacterLevelingPanel';

describe('CharacterLevelingPanel', () => {
  const mockCharacter = {
    name: 'Test Hero',
    advancement: {
      experiencePoints: 10,
      getRank: () => 'Novice',
      getAvailableAdvances: () => 2,
      spentAdvances: 1
    }
  };

  const mockOnUpdate = jest.fn();
  const mockOnAdvanceSpent = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Experience Display', () => {
    it('should display current experience points', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
        />
      );

      expect(screen.getByTestId('experience-display')).toHaveTextContent('10');
    });

    it('should display current rank', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
        />
      );

      expect(screen.getByTestId('character-rank')).toHaveTextContent('Novice');
    });

    it('should display available advances', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
        />
      );

      expect(screen.getByTestId('advances-available')).toHaveTextContent('2');
    });

    it('should show rank progression bar', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
        />
      );

      const progressBar = screen.getByTestId('rank-progress');
      expect(progressBar).toBeInTheDocument();
      expect(progressBar).toHaveAttribute('aria-valuenow', '10');
      expect(progressBar).toHaveAttribute('aria-valuemax', '20');
    });
  });

  describe('Adding Experience', () => {
    it('should show add experience input', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      const input = screen.getByTestId('add-experience');
      expect(input).toBeInTheDocument();
    });

    it('should add experience when button clicked', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      const input = screen.getByTestId('add-experience');
      const button = screen.getByTestId('add-xp-button');

      await userEvent.type(input, '5');
      await userEvent.click(button);

      expect(mockOnUpdate).toHaveBeenCalledWith({
        ...mockCharacter,
        advancement: expect.objectContaining({
          experiencePoints: 15
        })
      });
    });

    it('should validate experience input is positive', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      const input = screen.getByTestId('add-experience');
      const button = screen.getByTestId('add-xp-button');

      await userEvent.type(input, '-5');
      await userEvent.click(button);

      expect(screen.getByText(/must be positive/i)).toBeInTheDocument();
      expect(mockOnUpdate).not.toHaveBeenCalled();
    });

    it('should clear input after adding experience', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      const input = screen.getByTestId('add-experience');
      const button = screen.getByTestId('add-xp-button');

      await userEvent.type(input, '5');
      await userEvent.click(button);

      expect(input).toHaveValue('');
    });
  });

  describe('Spending Advances', () => {
    it('should show spend advance button when advances available', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      expect(screen.getByTestId('spend-advance')).toBeInTheDocument();
    });

    it('should disable spend advance button when no advances available', () => {
      const noAdvancesCharacter = {
        ...mockCharacter,
        advancement: {
          ...mockCharacter.advancement,
          getAvailableAdvances: () => 0
        }
      };

      render(
        <CharacterLevelingPanel 
          character={noAdvancesCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      expect(screen.getByTestId('spend-advance')).toBeDisabled();
    });

    it('should open advance options modal when spend clicked', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      const button = screen.getByTestId('spend-advance');
      await userEvent.click(button);

      expect(screen.getByTestId('advance-options')).toBeInTheDocument();
    });

    it('should show advance type options in modal', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));

      expect(screen.getByTestId('advance-attribute')).toBeInTheDocument();
      expect(screen.getByTestId('advance-skills')).toBeInTheDocument();
      expect(screen.getByTestId('advance-edge')).toBeInTheDocument();
      expect(screen.getByTestId('advance-new-skill')).toBeInTheDocument();
    });
  });

  describe('Attribute Advancement', () => {
    it('should show attribute options when selected', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-attribute'));

      expect(screen.getByTestId('select-agility')).toBeInTheDocument();
      expect(screen.getByTestId('select-smarts')).toBeInTheDocument();
      expect(screen.getByTestId('select-spirit')).toBeInTheDocument();
      expect(screen.getByTestId('select-strength')).toBeInTheDocument();
      expect(screen.getByTestId('select-vigor')).toBeInTheDocument();
    });

    it('should show current and next die values', async () => {
      const character = {
        ...mockCharacter,
        agility: { die: 'd4' }
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-attribute'));
      await userEvent.click(screen.getByTestId('select-agility'));

      expect(screen.getByText(/d4 → d6/)).toBeInTheDocument();
    });

    it('should apply attribute advance', async () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          onAdvanceSpent={mockOnAdvanceSpent}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-attribute'));
      await userEvent.click(screen.getByTestId('select-agility'));
      await userEvent.click(screen.getByTestId('apply-advance'));

      expect(mockOnAdvanceSpent).toHaveBeenCalledWith({
        type: 'attribute',
        details: expect.objectContaining({
          attribute: 'agility'
        })
      });
    });
  });

  describe('Edge Advancement', () => {
    it('should show available edges when selected', async () => {
      const character = {
        ...mockCharacter,
        edges: [],
        availableEdges: ['Quick', 'Brawny', 'Alertness']
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-edge'));

      expect(screen.getByText('Quick')).toBeInTheDocument();
      expect(screen.getByText('Brawny')).toBeInTheDocument();
      expect(screen.getByText('Alertness')).toBeInTheDocument();
    });

    it('should filter edges by rank requirements', async () => {
      const character = {
        ...mockCharacter,
        edges: [],
        availableEdges: [
          { name: 'Quick', rank: 'Novice' },
          { name: 'Master Fighter', rank: 'Veteran' }
        ]
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-edge'));

      expect(screen.getByText('Quick')).toBeInTheDocument();
      expect(screen.queryByText('Master Fighter')).not.toBeInTheDocument();
    });

    it('should show edge requirements', async () => {
      const character = {
        ...mockCharacter,
        edges: [],
        availableEdges: [
          { 
            name: 'Fleet-Footed',
            requirements: 'Agility d6+'
          }
        ]
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-edge'));

      expect(screen.getByText(/Agility d6\+/)).toBeInTheDocument();
    });
  });

  describe('Skill Advancement', () => {
    it('should show skill options when selected', async () => {
      const character = {
        ...mockCharacter,
        skills: [
          { name: 'Fighting', die: 'd4' },
          { name: 'Notice', die: 'd6' }
        ]
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-skills'));

      expect(screen.getByTestId('skill-fighting')).toBeInTheDocument();
      expect(screen.getByTestId('skill-notice')).toBeInTheDocument();
    });

    it('should allow selecting two skills to advance', async () => {
      const character = {
        ...mockCharacter,
        skills: [
          { name: 'Fighting', die: 'd4' },
          { name: 'Notice', die: 'd4' },
          { name: 'Shooting', die: 'd4' }
        ]
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-skills'));

      const fighting = screen.getByTestId('skill-fighting');
      const notice = screen.getByTestId('skill-notice');
      const shooting = screen.getByTestId('skill-shooting');

      await userEvent.click(fighting);
      await userEvent.click(notice);
      await userEvent.click(shooting);

      // Third selection should be prevented
      expect(fighting).toBeChecked();
      expect(notice).toBeChecked();
      expect(shooting).not.toBeChecked();
    });

    it('should show new die values for selected skills', async () => {
      const character = {
        ...mockCharacter,
        skills: [
          { name: 'Fighting', die: 'd4' }
        ]
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('spend-advance'));
      await userEvent.click(screen.getByTestId('advance-skills'));
      await userEvent.click(screen.getByTestId('skill-fighting'));

      expect(screen.getByTestId('fighting-d6')).toBeInTheDocument();
    });
  });

  describe('Advance History', () => {
    it('should display advance history', () => {
      const character = {
        ...mockCharacter,
        advancement: {
          ...mockCharacter.advancement,
          advanceHistory: [
            { number: 1, type: 'attribute', description: 'Increased Vigor to d6', date: '2024-01-01' },
            { number: 2, type: 'edge', description: 'Added Edge: Brawny', date: '2024-01-15' }
          ]
        }
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
        />
      );

      expect(screen.getByText(/Increased Vigor to d6/)).toBeInTheDocument();
      expect(screen.getByText(/Added Edge: Brawny/)).toBeInTheDocument();
    });

    it('should show undo button for last advance in edit mode', () => {
      const character = {
        ...mockCharacter,
        advancement: {
          ...mockCharacter.advancement,
          advanceHistory: [
            { number: 1, type: 'attribute', description: 'Increased Vigor to d6' }
          ]
        }
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      expect(screen.getByTestId('undo-last-advance')).toBeInTheDocument();
    });

    it('should undo last advance when confirmed', async () => {
      const character = {
        ...mockCharacter,
        advancement: {
          ...mockCharacter.advancement,
          advanceHistory: [
            { number: 1, type: 'attribute', description: 'Increased Vigor to d6' }
          ]
        }
      };

      render(
        <CharacterLevelingPanel 
          character={character}
          onUpdate={mockOnUpdate}
          editMode={true}
        />
      );

      await userEvent.click(screen.getByTestId('undo-last-advance'));
      await userEvent.click(screen.getByTestId('confirm-undo'));

      expect(mockOnUpdate).toHaveBeenCalledWith(
        expect.objectContaining({
          advancement: expect.objectContaining({
            advanceHistory: []
          })
        })
      );
    });
  });

  describe('Rank Requirements Display', () => {
    it('should show rank requirements table', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          showRequirements={true}
        />
      );

      expect(screen.getByText('Novice')).toBeInTheDocument();
      expect(screen.getByText('Seasoned')).toBeInTheDocument();
      expect(screen.getByText('Veteran')).toBeInTheDocument();
      expect(screen.getByText('Heroic')).toBeInTheDocument();
      expect(screen.getByText('Legendary')).toBeInTheDocument();
    });

    it('should highlight current rank', () => {
      render(
        <CharacterLevelingPanel 
          character={mockCharacter}
          onUpdate={mockOnUpdate}
          showRequirements={true}
        />
      );

      const noviceRow = screen.getByTestId('rank-row-novice');
      expect(noviceRow).toHaveClass('current-rank');
    });
  });
});