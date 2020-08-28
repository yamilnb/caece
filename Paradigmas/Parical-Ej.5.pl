%Ej 5.i
cantll([],0). %[H1]
cantll([X|XS],S) :- not(is_list(X)), cantll(XS,S). %[F1]
cantll([[]|XSS],S) :- cantll(XSS,S). %[F2]
cantll([[X|XS]|XSS],S) :- is_list([X|XS]), cantll([XS|XSS],S2), S is S2 + 1.  %[F3]

% Ej 5.ii
% cantll([[5,a],w,[],[8,4,j,3]], X)
    % [F1]  {X/[5,a]} 
        % not(is_list(X)) falla

    % [F2]  [5,a] no unifica con []

    % [F3]  {X/5, XS/[a], S/X } 
        % is_list([X|XS]) verdadero.
        % S is S2 + 1 {S2/X-1}
        % cantll([XS|XSS],S2) {XS/[a], XSS/[w,[],[8,4,j,3]]], S2/X-1}
            % [F1]  {X/[a]} 
                % not(is_list(X)) falla

            % [F2]  [a] no unifica con []

            % [F3]  {X/[a], XS/[], S/X-1 }
                % is_list([X|XS]) verdadero
                % S is S2 + 1 {S2/X-2}
                % cantll([XS|XSS],S2) {XS/[], XSS/[w,[],[8,4,j,3]]], S2/X-2}

                    % [F1]  {X/[]} 
                        % not(is_list(X)) falla

                    % [F2]  {XSS/[w,[],[8,4,j,3]],S/X-2}
                        % cantll(XSS,S)

                            % [F1] {X/w, XS/[[],[8,4,j,3]], S/X-2} 
                                % not(is_list(X)) verdadero
                                % cantll(XS,S) {XS/[[],[8,4,j,3]], S/X-2} 
                                    % [F1] [] no unifica con [X|XS]

                                    % [F2] {XSS/[[8,4,j,3]], S/X-2}
                                        % cantll(XSS,S)
                                            % [F1] {X/[8,4,j,3] XS/[[]], S/X-2}
                                                % not(is_list(X)) falso

                                            % [F2] [8,4,j,3]  no unifica con []
 
                                            % [F3] {X/8, XS/[4,j,3], XSS/[[]],S/X-2)}
                                                % is_list([X|XS]) verdadero 
                                                % S is S2 + 1. {S2/X-3}
                                                % cantll([XS|XSS],S2) {XS/[4,j,3], XSS/[[]], S2/X-3}                                                    
                                                    % [F1]  {X/[4,j,3] XS/[[]], S/X-2}
                                                        % not(is_list(X)) falso

                                                    % [F2]  [4,j,3] no unifica con []

                                                    % [F3]  {X/4, XS/[j,3], S/X-3 } 
                                                        % is_list([X|XS]) verdadero
                                                        % S is S2 + 1. {S2/X-4}
                                                        % cantll([XS|XSS],S2) {XS/[j,3], XSS/[[]], S2/X-4}
                                                            % [F1]  {X/[j,3] XS/[[]], S/X-2}
                                                                % not(is_list(X)) falso

                                                            % [F2]  [j,3] no unifica con []

                                                            % [F3]  {X/j, XS/[3], S/X-4 } 
                                                                % is_list([X|XS]) verdadero
                                                                % S is S2 + 1. {S2/X-5}
                                                                % cantll([XS|XSS],S2) {XS/[3], XSS/[[]], S2/X-5}
                                                                    % [F1]  {X/[3] XS/[[]], S/X-2}
                                                                        % not(is_list(X)) falso

                                                                    % [F2]  [3] no unifica con []

                                                                    % [F3]  {X/3, XS/[], S/X-5 } 
                                                                        % % is_list([X|XS]) verdadero
                                                                        % S is S2 + 1. {S2/X-6}
                                                                        % cantll([XS|XSS],S2) {XS/[], XSS/[], S/X-6}
                                                                            % [H1] X-6 = 0 => X=6 cláusula nula
                                    % [F3] [] no unfica con  [X|XS]  

                            % [F2] w no unfica con [] falla

                            % [F3] w no unfica con [X|XS]

                    % [F3] [] no unifica con [X|XS]

% Resultado X=6
