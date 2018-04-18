package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.Persona;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.List;

public interface CharacterRepository extends PagingAndSortingRepository<Persona, Long> {
	List<Persona> findByName(String name);
}
