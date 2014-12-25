package org.savageworlds.repository;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;

import org.savageworlds.game.model.ArmorDescription;

import jdo.core.repository.RepositoryTemplate;

public class ArmorDescriptionRepository extends RepositoryTemplate<ArmorDescription, Long> {

	public ArmorDescriptionRepository() {
		super(ArmorDescription.class);		
	}

	@PersistenceContext(name = "SavageWorlds")
	protected EntityManager em;
	
	@Override
	protected EntityManager em() {
		return em;
	}

}
