import React from 'react'
import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import SceneEditor from './SceneEditor'

describe('SceneEditor', () => {
  const mockScene = {
    id: '1',
    name: 'Opening Tavern Scene',
    description: 'The heroes gather at the Red Dragon Inn',
    dramatis_personae: [
      { name: 'Sir Gareth', description: 'A noble knight' },
      { name: 'Mara', description: 'A cunning thief' }
    ]
  }

  const mockAvailableCharacters = [
    { name: 'Sir Gareth', description: 'A noble knight' },
    { name: 'Mara', description: 'A cunning thief' },
    { name: 'Grimjaw', description: 'An orc bartender' },
    { name: 'Eldara', description: 'An elven mage' }
  ]

  describe('rendering', () => {
    test('should render scene editor form', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} />)
      
      expect(screen.getByDisplayValue('Opening Tavern Scene')).toBeInTheDocument()
      expect(screen.getByDisplayValue('The heroes gather at the Red Dragon Inn')).toBeInTheDocument()
      expect(screen.getByText('Save Scene')).toBeInTheDocument()
    })

    test('should render empty form for new scene', () => {
      const newScene = { id: '', name: '', description: '', dramatis_personae: [] }
      render(<SceneEditor scene={newScene} availableCharacters={mockAvailableCharacters} />)
      
      expect(screen.getByPlaceholderText('Scene Name')).toHaveValue('')
      expect(screen.getByPlaceholderText('Scene Description')).toHaveValue('')
    })

    test('should render dramatis personae list', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} />)
      
      expect(screen.getByText('Dramatis Personae')).toBeInTheDocument()
      expect(screen.getAllByText('Sir Gareth')).toHaveLength(2) // One in table, one in dropdown
      expect(screen.getAllByText('Mara')).toHaveLength(2) // One in table, one in dropdown
    })

    test('should render character selection dropdown', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} />)
      
      expect(screen.getByText('Add Character')).toBeInTheDocument()
      expect(screen.getByRole('combobox')).toBeInTheDocument()
    })
  })

  describe('form interactions', () => {
    test('should update scene name when input changes', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const nameInput = screen.getByDisplayValue('Opening Tavern Scene')
      fireEvent.change(nameInput, { target: { value: 'Updated Scene Name' } })
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          name: 'Updated Scene Name'
        }))
      })
    })

    test('should update scene description when textarea changes', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const descriptionTextarea = screen.getByDisplayValue('The heroes gather at the Red Dragon Inn')
      fireEvent.change(descriptionTextarea, { target: { value: 'Updated description' } })
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          description: 'Updated description'
        }))
      })
    })

    test('should call onSave when save button clicked', () => {
      const onSave = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onSave={onSave} />)
      
      const saveButton = screen.getByText('Save Scene')
      fireEvent.click(saveButton)
      
      expect(onSave).toHaveBeenCalledWith(mockScene)
    })
  })

  describe('character management', () => {
    test('should add character to dramatis personae', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const select = screen.getByRole('combobox')
      fireEvent.change(select, { target: { value: 'Grimjaw' } })
      
      const addButton = screen.getByText('Add Character')
      fireEvent.click(addButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.arrayContaining([
            expect.objectContaining({ name: 'Grimjaw' })
          ])
        }))
      })
    })

    test('should remove character from dramatis personae', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const removeButtons = screen.getAllByText('Remove')
      fireEvent.click(removeButtons[0])
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.not.arrayContaining([
            expect.objectContaining({ name: 'Sir Gareth' })
          ])
        }))
      })
    })

    test('should update character description', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const editButtons = screen.getAllByText('Edit')
      fireEvent.click(editButtons[0])
      
      const descriptionInput = screen.getByDisplayValue('A noble knight')
      fireEvent.change(descriptionInput, { target: { value: 'The party leader' } })
      
      const saveCharacterButton = screen.getByText('Save Character')
      fireEvent.click(saveCharacterButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.arrayContaining([
            expect.objectContaining({ 
              name: 'Sir Gareth',
              description: 'The party leader'
            })
          ])
        }))
      })
    })
  })

  describe('validation', () => {
    test('should show error for empty scene name', () => {
      const sceneWithoutName = { ...mockScene, name: '' }
      render(<SceneEditor scene={sceneWithoutName} availableCharacters={mockAvailableCharacters} />)
      
      const saveButton = screen.getByText('Save Scene')
      fireEvent.click(saveButton)
      
      expect(screen.getByText('Scene name is required')).toBeInTheDocument()
    })

    test('should prevent adding duplicate characters', () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} onChange={onChange} />)
      
      const select = screen.getByRole('combobox')
      fireEvent.change(select, { target: { value: 'Sir Gareth' } })
      
      const addButton = screen.getByText('Add Character')
      fireEvent.click(addButton)
      
      expect(screen.getByText('Character is already in the scene')).toBeInTheDocument()
      expect(onChange).not.toHaveBeenCalled()
    })
  })

  describe('error handling', () => {
    test('should handle missing availableCharacters prop', () => {
      render(<SceneEditor scene={mockScene} />)
      
      expect(screen.getByDisplayValue('Opening Tavern Scene')).toBeInTheDocument()
    })

    test('should handle null scene prop', () => {
      const emptyScene = { id: '', name: '', description: '', dramatis_personae: [] }
      render(<SceneEditor scene={null} availableCharacters={mockAvailableCharacters} />)
      
      // Should render with default empty scene
      expect(screen.getByPlaceholderText('Scene Name')).toBeInTheDocument()
    })
  })
})