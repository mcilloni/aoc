#!/usr/bin/env php
<?php declare(strict_types=1);

# AoC 2024 Day 17

enum JumpKind {
    case Abs;
    case Rel;
}

class Cpu {
    private int $a, $b, $c;
    private string $outbuf = '';

    public function __construct(array $initialState) {
        foreach ($initialState as $reg => $val) {
            $key = strtolower($reg);

            $this->$key = $val;
        }
    }

    public function execute(array $text): string {
        static $logic = [
            'adv',
            'bxl',
            'bst',
            'jnz',
            'bxc',
            'out',
            'bdv',
            'cdv',
        ];

        $ip = 0;
        $len = count($text);
    
        while ($ip < $len - 1) {
            $opcode = $text[$ip];
            if (!($opcode instanceof Opcode)) {
                throw new ValueError("invalid opcode at $ip");
            }
    
            $arg = $text[$ip + 1];

            $opc = $logic[$opcode->value];
    
            [$kind, $offset] = $this->$opc($arg);
    
            $ip = match ($kind) {
                JumpKind::Abs => $arg,
                JumpKind::Rel => $ip + $offset,
            };
        }

        return $this->output();
    }

    public function output(): string {
        return $this->outbuf;
    }

    public function resolve(int $imm): int {
        if ($imm < 0) {
            throw new ValueError("negative immediate value");
        } elseif ($imm <= 3) {
            return $imm;
        } elseif ($imm == 4) {
            return $this->a;
        } elseif ($imm == 5) {
            return $this->b;
        } elseif ($imm == 6) {
            return $this->c;
        } elseif  ($imm == 7) {
            throw new ValueError("invalid immediate value 7");
        } else {
            throw new ValueError("immediate value too large");
        }
    }
    
    private function adv(int $combo): array{
        $op1 = $this->a;
        $op2 = $this->resolve($combo);
        
        $this->a = $op1 >> $op2;
        
        return [JumpKind::Rel, 2];
    }
    
    function bxl(int $imm): array {
        $this->b ^= $imm;
        
        return [JumpKind::Rel, 2];
    }
    
    function bst(int $combo): array {
        $this->b = $this->resolve($combo) & 0b111;
        
        return [JumpKind::Rel, 2];
    }
    
    function jnz(int $imm): array {
        return $this->a === 0
        ? [JumpKind::Rel, 2]
        : [JumpKind::Abs, $imm];
    }
    
    function bxc(int $imm): array {
        ($imm); // unused
        
        $this->b ^= $this->c;
        
        return [JumpKind::Rel, 2];
    }
    
    function out(int $combo): array {
        $op = $this->resolve($combo) & 0b111;
        
        $this->outbuf .= $op . ',';
        
        return [JumpKind::Rel, 2];
    }
    
    function bdv(int $combo): array {
        $op1 = $this->a;
        $op2 = $this->resolve($combo);
        
        $this->b = $op1 >> $op2;
        
        return [JumpKind::Rel, 2];
    }
    
    function cdv(int $combo): array {
        $op1 = $this->a;
        $op2 = $this->resolve($combo);
        
        $this->c = $op1 >> $op2;
        
        return [JumpKind::Rel, 2];
    }
}

enum Opcode : int {
    case Adv = 0;
    case Bxl = 1;
    case Bst = 2;
    case Jnz = 3;
    case Bxc = 4;
    case Out = 5;
    case Bdv = 6;
    case Cdv = 7;
}

function parse_input(string $fname): array|false {
    $file = fopen($fname, "r");
    if (!$file) {
        return false;
    }

    $regs = [];
    $prog = null;
    $rawp = '';

    while (($line = fgets($file)) !== false) {
        if (preg_match('/Register\s+([\w]):\s*(\d+)/', $line, $matches)) {
            $regs[$matches[1]] = (int)$matches[2];
        } else if (preg_match('/Program:\s*([\d,]+)/', $line, $matches)) {
            $rawp = $matches[1];
            $prog = array_map('intval', explode(',', $rawp));
        }
    }

    fclose($file);

    if (empty($regs) || empty($prog)) {
        return false;
    }

    $len = count($prog);
    for ($i = 0; $i < $len; $i += 2) {
        $prog[$i] = Opcode::from($prog[$i]);
    }

    return [$regs, $prog, $rawp];
}

function main(string $script, array $args) {
    if (count($args) != 1) {
        fwrite(STDERR, "error: wrong number of arguments\nusage: $script <input_file>\n");

        exit(2);
    }

    $input = parse_input($args[0]);
    if ($input === false) {
        fwrite(STDERR, "error: failed to parse input\n");

        exit(1);
    }

    [$regs, $prog, $rawp] = $input;

    echo 'part1: ' . rtrim((new Cpu($regs))->execute($prog), ',') . PHP_EOL;
}

main($argv[0], array_slice($argv, 1));
